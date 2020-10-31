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

    static function addClass(array $details)
    {
        $required = array(
            'name',
            'year',
            'days',
            'timeslot',
            'duration',
            'active'
        );
        foreach ($required as $field) {
            if (!isset($details[$field])) {
                return false;
            }
        }
        foreach ($details as $key => $value) {
            if (!in_array($key, $required)) {
                return false;
            }
        }
        $details['sessions'] = [];
        $details['present'] = [];
        $db = (new MongoDB\Client(connect_string))->selectDatabase('elmportal1');
        $collection = $db->selectCollection('classes');
        $result = $collection->insertOne($details);
        return (string) $result->getInsertedId();
    }

    static function updateClassDetails($id, $data)
    {
        $db = (new MongoDB\Client(connect_string))->selectDatabase('elmportal1');
        $collection = $db->selectCollection('classes');
        $update = array();
        if (isset($data['name']))
            $update['name'] = (string) $data['name'];
        if (isset($data['year']))
            $update['year'] = intval($data['year']);
        if (isset($data['timeslot']))
            $update['timeslot'] = (string) $data['timeslot'];
        if (isset($data['duration']))
            $update['duration'] = floatval($data['duration']);
        if (isset($data['days']))
            $update['days'] = $data['days'];
        if (isset($data['active']))
            $update['active'] = boolval($data['active']);
        $collection->updateOne(
            array('_id' => new \MongoDB\BSON\ObjectId($id)),
            array('$set' => $update)
        );
        return true;
    }

    static function addSession($id, $data)
    {
        $db = (new MongoDB\Client(connect_string))->selectDatabase('elmportal1');
        $collection = $db->selectCollection('classes');
        $data['_id'] = new \MongoDB\BSON\ObjectId();
        $result = $collection->updateOne(
            array('_id' => new \MongoDB\BSON\ObjectId($id)),
            array('$push' => array('sessions' => $data))
        );
        return (string) $data['_id'];
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
            array('$addToSet' => array('tutors' => array(
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
        $tutorList = !is_null($tutorList) && isset($tutorList['tutors'])
            ? iterator_to_array($tutorList['tutors'], false) : array();

        $tutorList = array_map(function ($tutor) {
            $tutor['id'] = (string) $tutor['id'];
            $tutorFull = DBTutor::getTutor($tutor['id']);
            $tutor['name'] = $tutorFull['name'];
            $tutor['admin'] = $tutorFull['admin'];
            $tutor['joinDate'] = $tutor['joinedOn']->toDateTime()->format('Y-m-d');
            unset($tutor['joinedOn']);
            return $tutor;
        }, $tutorList);
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
        $tutorList = !is_null($tutorList) && isset($tutorList['tutors'])
            ? $tutorList['tutors'] : array();
        $tutorList = iterator_to_array($tutorList, false);
        $tutorList = array_map(function ($elem) {
            return $elem['id'];
        }, $tutorList);
        return $tutorList;
    }

    static function isTutorInClass($id, $tutorId)
    {
        $tutorList = DBClass::getTutorIds($id);
        foreach ($tutorList as $tutorElemId) {
            if ((string) $tutorElemId == $tutorId) {
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

    static function sessionTutors($id, $sessionId)
    {
        $db = (new MongoDB\Client(connect_string))->selectDatabase('elmportal1');
        $collection = $db->selectCollection('classes');
        $sessionList = $collection->findOne(
            array(
                '_id' => new MongoDB\BSON\ObjectId($id),
                'sessions._id' => new \MongoDB\BSON\ObjectId($sessionId)
            ),
            array('projections' => array(
                'tutors' => 1,
                'tutors.id' => 1,
                'tutors.joinedOn' => 1,
                'tutors.leftOn' => 1,
                'sessions' => 1,
                'sessions._id' => 1,
                'sessions.date' => 1,
                'sessions.present' => 1,
            ))
        );
        if (is_null($sessionList) || !isset($sessionList['sessions']) || count($sessionList['sessions']) == 0) {
            return array();
        }
        if (!isset($sessionList['tutors'])) {
            return array();
        }
        $tutors = $sessionList['tutors'];
        $sessionDate = $sessionList['sessions'][0]['date'];
        $tutors = iterator_to_array($tutors);
        $tutors = array_filter($tutors, function ($elem) use ($sessionDate) {
            $joinedOn = $elem['joinedOn'];
            $tutorFull = DBTutor::getTutor((string) $elem['id']);
            $elem['name'] = $tutorFull['name'];
            $elem['admin'] = $tutorFull['admin'];
            if ($sessionDate < $joinedOn) {
                return false;
            }
            if (isset($elem['leftOn'])) {
                if ($sessionDate > $elem['leftOn']) {
                    return false;
                }
            }
            return true;
        });
        return $tutors;
    }

    static function tutorsAbsent($id, $sessionId)
    {
        $allTutors = DBClass::sessionTutors($id, $sessionId);
        $allTutors = array_map(function ($elem) {
            return $elem['id'];
        }, $allTutors);
        $present = DBClass::tutorsPresent($id, $sessionId);
        $absent = array_filter($allTutors, function ($elem) use ($present) {
            return !in_array($elem, $present);
        });
        return $absent;
    }

    static function tutorsPresent($id, $sessionId)
    {
        $db = (new MongoDB\Client(connect_string))->selectDatabase('elmportal1');
        $collection = $db->selectCollection('classes');
        $sessionList = $collection->findOne(
            array(
                '_id' => new MongoDB\BSON\ObjectId($id)
            ),
            array('projections' => array(
                'sessions' => array('$elemMatch' => array('_id' => array('$eq' => new \MongoDB\BSON\ObjectId($sessionId)))),
                'tutors' => 0
            ))
        );
        // seems to be a bug in the PHP mongodb driver that the $elemMatch projection does not filter the output array
        if (is_null($sessionList) || !isset($sessionList['sessions'])) {
            return array();
        }
        $sessionList = iterator_to_array($sessionList['sessions']);
        $sessionList = array_filter($sessionList, function ($elem) use ($sessionId) {
            return $elem['_id'] == new \MongoDB\BSON\ObjectId($sessionId);
        });
        $sessionList = array_values($sessionList);
        if (count($sessionList) != 1) {
            return array();
        }
        $session = $sessionList[0];
        if (!isset($session['present'])) {
            return array();
        }
        $tutors = iterator_to_array($session['present']);
        return $tutors;
    }

    static function markPresent($id, $sessionId, $tutorId)
    {
        if (!DBTutor::isValidTutor($tutorId)) {
            return false;
        }
        if (!DBClass::isTutorInClass($id, $tutorId)) {
            return false;
        }
        $db = (new MongoDB\Client(connect_string))->selectDatabase('elmportal1');
        $collection = $db->selectCollection('classes');
        $collection->updateOne(
            array(
                '_id' => new \MongoDB\BSON\ObjectId($id),
                'sessions._id' => new \MongoDB\BSON\ObjectId($sessionId),
                'sessions.present' => array('$exists' => false)
            ),
            array('$set' => array('sessions.$.present' => array()))
        );
        $collection->updateOne(
            array(
                '_id' => new \MongoDB\BSON\ObjectId($id),
                'sessions._id' => new \MongoDB\BSON\ObjectId($sessionId)
            ),
            array('$addToSet' => array('sessions.$.present' => new \MongoDB\BSON\ObjectId($tutorId)))
        );
        return true;
    }

    static function markAbsent($id, $sessionId, $tutorId)
    {
        if (!DBTutor::isValidTutor($tutorId)) {
            return false;
        }
        if (!DBClass::isTutorInClass($id, $tutorId)) {
            return false;
        }
        $db = (new MongoDB\Client(connect_string))->selectDatabase('elmportal1');
        $collection = $db->selectCollection('classes');
        $collection->updateOne(
            array(
                '_id' => new \MongoDB\BSON\ObjectId($id),
                'sessions._id' => new \MongoDB\BSON\ObjectId($sessionId),
                'sessions.present' => array('$exists' => false)
            ),
            array('$set' => array('sessions.$.present' => array()))
        );
        $collection->updateOne(
            array(
                '_id' => new \MongoDB\BSON\ObjectId($id),
                'sessions._id' => new \MongoDB\BSON\ObjectId($sessionId)
            ),
            array('$pull' => array('sessions.$.present' => new \MongoDB\BSON\ObjectId($tutorId)))
        );
        return true;
    }

    static function getSession($id, $sessionId)
    {
        $db = (new MongoDB\Client(connect_string))->selectDatabase('elmportal1');
        $collection = $db->selectCollection('classes');
        $sessionList = $collection->findOne(
            array(
                '_id' => new MongoDB\BSON\ObjectId($id),
                'sessions._id' => new \MongoDB\BSON\ObjectId($sessionId)
            ),
            array('projections' => array(
                'sessions' => 1,
                'sessions._id' => 1,
                'sessions.date' => 1,
                'sessions.duration' => 1,
                'sessions.remarks' => 1
            ))
        );
        if (is_null($sessionList) || !isset($sessionList['sessions'])) {
            return array();
        }
        $sessionList = iterator_to_array($sessionList['sessions']);
        $session = $sessionList[0];
        return $session;
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
