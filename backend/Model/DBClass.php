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
}
