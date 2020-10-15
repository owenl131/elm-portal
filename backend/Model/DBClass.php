<?php

class DBClass
{
    static function processFilters(array $filters)
    {
        $result = array();
        if (isset($filters['name'])) {
            $result['name'] = array('$in' => array_map(function (string $filterString) {
                return new \MongoDB\BSON\Regex(preg_quote($filterString), 'i');
            }, $filters['name']));
        }
        if (isset($filters['days'])) {
            $result['days'] = array(
                '$elemMatch' => array(
                    '$in' => array_map('intval', $filters['days'])
                )
            );
        }
        if (isset($filters['yearLower']) && isset($filters['yearUpper'])) {
            $result['year'] = array(
                '$gte' => intval($filters['yearLower'][0]),
                '$lte' => intval($filters['yearUpper'][0])
            );
        } else if (isset($filters['yearLower'])) {
            $result['year'] = array(
                '$gte' => intval($filters['yearLower'][0])
            );
        } else if (isset($filters['yearUpper'])) {
            $result['year'] = array(
                '$lte' => intval($filters['yearUpper'][0])
            );
        }
        return $result;
    }

    static function getClassList(int $page, array $filters, int $perPage = 20)
    {
        $db = (new MongoDB\Client(connect_string))->selectDatabase('elmportal1');
        $collection = $db->selectCollection('classes');
        $filterBy = DBClass::processFilters($filters);
        $numResults = $collection->countDocuments($filterBy);
        $result = $collection->find(
            $filterBy,
            array(
                'projection' => array(
                    'sessions' => 0,
                    'tutors' => 0
                ),
                'skip' => $page * $perPage,
                'limit' => $perPage
            )
        );
        return array(
            'data' => $result->toArray(),
            'total' => $numResults,
            'perPage' => $perPage,
            'page' => $page
        );
    }


    static function getClass($id)
    {
        $db = (new MongoDB\Client(connect_string))->selectDatabase('elmportal1');
        $collection = $db->selectCollection('classes');
        return $collection->findOne(
            array('_id' => new MongoDB\BSON\ObjectId($id))
        );
    }



    static function addTutor($id, $tutorId, $joinDate)
    {
        if (!DBTutor::isValidTutor($tutorId)) {
            return false;
        }
        if (DBClass::isTutorInClass($id, $tutorId)) {
            return false;
        }
        $db = (new MongoDB\Client(connect_string))->selectDatabase('elmportal1');
        $collection = $db->selectCollection('classes');
        $collection->updateOne(
            array('_id' => new MongoDB\BSON\ObjectId($id)),
            array('$addToSet' => array('tutors', array(
                'id' => new \MongoDB\BSON\ObjectId($tutorId),
                'joinedOn' => new \MongoDB\BSON\UTCDateTime(strtotime($joinDate) * 1000)
            )))
        );
        return true;
    }

    static function getTutors($id)
    {
        $db = (new MongoDB\Client(connect_string))->selectDatabase('elmportal1');
        $collection = $db->selectCollection('classes');
        $tutorList = $collection->findOne(
            array('_id' => new MongoDB\BSON\ObjectId($id)),
            array('projections' => array('tutors' => 1))
        );

        $tutorList = !is_null($tutorList) && isset($tutorList['tutors']) ? $tutorList['tutors'] : array();
        foreach ($tutorList as $tutor) {
            $tutorFull = DBTutor::getTutor((string) $tutor['id']);
            $tutor['name'] = $tutorFull['name'];
            $tutor['admin'] = $tutorFull['admin'];
        }
        return $tutorList;
    }

    static function getTutorIds($id)
    {
        $db = (new MongoDB\Client(connect_string))->selectDatabase('elmportal1');
        $collection = $db->selectCollection('classes');
        $tutorList = $collection->findOne(
            array('_id' => new MongoDB\BSON\ObjectId($id)),
            array('projections' => array('tutors' => 1, 'tutors.id' => 1))
        );

        $tutorList = !is_null($tutorList) && isset($tutorList['tutors']) ? $tutorList['tutors'] : array();
        $tutorList = array_map(function ($elem) {
            return $elem['id'];
        }, $tutorList);
        return $tutorList;
    }

    static function isTutorInClass($id, $tutorId)
    {
        $tutorList = DBClass::getTutorIds($id);
        foreach ($tutorList as $tutor) {
            if ((string) $tutor['id'] == $tutorId) {
                return true;
            }
        }
        return false;
    }

    static function removeTutor($id, $tutorId)
    {
        if (!DBTutor::isValidTutor($tutorId)) {
            return false;
        }
        $db = (new MongoDB\Client(connect_string))->selectDatabase('elmportal1');
        $collection = $db->selectCollection('classes');
        $collection->updateOne(
            array('_id' => new MongoDB\BSON\ObjectId($id)),
            array('$pull' => array('tutors', array(
                'id' => new \MongoDB\BSON\ObjectId($tutorId)
            )))
        );
        return true;
    }

    static function inactivateTutor($id, $tutorId, $leaveDate)
    {
        if (!DBTutor::isValidTutor($tutorId)) {
            return false;
        }
        $db = (new MongoDB\Client(connect_string))->selectDatabase('elmportal1');
        $collection = $db->selectCollection('classes');
        $collection->updateOne(
            array(
                '_id' => new MongoDB\BSON\ObjectId($id),
                'tutors.id' => new MongoDB\BSON\ObjectId($tutorId)
            ),
            array(
                '$set' => array(
                    'tutors.$.leftOn' => new MongoDB\BSON\UTCDateTime(strtotime($leaveDate) * 1000)
                )
            )
        );
        return true;
    }

    static function getSessions($id)
    {
        $db = (new MongoDB\Client(connect_string))->selectDatabase('elmportal1');
        $collection = $db->selectCollection('classes');
        $sessionList = $collection->findOne(
            array('_id' => new MongoDB\BSON\ObjectId($id)),
            array('projections' => array(
                'sessions' => 1,
                'sessions._id' => 1,
                'sessions.date' => 1,
                'sessions.duration' => 1,
                'sessions.remarks' => 1
            ))
        );
        $sessionList = !is_null($sessionList) && isset($sessionList['sessions'])
            ? $sessionList['sessions'] : array();
        return $sessionList;
    }

    static function tutorSuggestions($id, $filter)
    {
        $tutorsToAvoid = DBClass::getTutorIds($id);
        $db = (new MongoDB\Client(connect_string))->selectDatabase('elmportal1');
        $collection = $db->selectCollection('tutors');
        $results = $collection->find(
            array(
                '_id' => array('$nin' => $tutorsToAvoid),
                'name' => new \MongoDB\BSON\Regex($filter, 'i')
            ),
            array(
                'projection' => array(
                    'name' => 1,
                    'admin' => 1,
                    '_id' => 1
                ),
                'limit' => 20
            )
        )->toArray();
        return $results;
    }
}
