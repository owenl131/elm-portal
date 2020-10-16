<?php

require __DIR__ . '/../../vendor/autoload.php';
require __DIR__ . '/../config.php';

class DBTutor
{
    static function authenticateBySessionId(string $sessionId)
    {
        $db = (new MongoDB\Client(connect_string))->selectDatabase('elmportal1');
        $collection = $db->selectCollection('tutors');
        $result = $collection->countDocuments(array(
            'sessionId' => new \MongoDB\BSON\ObjectId($sessionId),
            'sessionExpiry' => array('$exists' => 1, '$gte' => new \MongoDB\BSON\UTCDateTime())
        ));
        if ($result == 1) {
            $tutor = $collection->findOne(
                array('sessionId' => new \MongoDB\BSON\ObjectId($sessionId)),
                array('projection' => array('_id' => 1))
            );
            return (string) $tutor['_id'];
        } else {
            return false;
        }
    }

    static function authenticateByCredentials(string $email, string $password)
    {
        $db = (new MongoDB\Client(connect_string))->selectDatabase('elmportal1');
        $collection = $db->selectCollection('tutors');
        if ($collection->countDocuments(array('email' => $email)) == 1) {
            $result = $collection->findOne(
                array('email' => $email),
                array('projection' => array('_id' => 1, 'password' => 1))
            );
            $isValid = password_verify($password, $result['password']);
            if ($isValid) {
                if ($collection->countDocuments(array('email' => $email, 'sessionExpiry' => array('$gte' => new \MongoDB\BSON\UTCDateTime()))) == 0) {
                    $collection->updateOne(
                        array('_id' => $result['_id']),
                        array(
                            '$set' => array(
                                'sessionId' => new \MongoDB\BSON\ObjectId(),
                                'sessionExpiry' => new \MongoDB\BSON\UTCDateTime((time() + 3600) * 1000)
                                // Session lasts for 1 hour
                            )
                        )
                    );
                }
                $updated = $collection->findOne(
                    array('_id' => $result['_id']),
                    array('projection' => array('sessionId' => 1, 'sessionExpiry' => 1))
                );
                return array(
                    'session' => (string) $updated['sessionId'],
                    'sessionExpiry' => $updated['sessionExpiry']->toDateTime()->getTimestamp()
                );
            }
        }
        return false;
    }

    static function isAdmin(string $sessionId)
    {
        $db = (new MongoDB\Client(connect_string))->selectDatabase('elmportal1');
        $collection = $db->selectCollection('tutors');
        $result = $collection->countDocuments(array(
            'sessionId' => new \MongoDB\BSON\ObjectId($sessionId),
            'sessionExpiry' => array('$lte', new MongoDB\BSON\UTCDateTime(time() * 1000)),
            'admin' => 0
        ));
        return $result == 1;
    }

    static function isLeaderAndAbove(string $sessionId)
    {
        $db = (new MongoDB\Client(connect_string))->selectDatabase('elmportal1');
        $collection = $db->selectCollection('tutors');
        $result = $collection->countDocuments(array(
            'sessionId' => new \MongoDB\BSON\ObjectId($sessionId),
            'sessionExpiry' => array('$lte', new MongoDB\BSON\UTCDateTime(time() * 1000)),
            'admin' => array('$lte' => 1)
        ));
        return $result == 1;
    }

    static function addTutor(array $details)
    {
        // ensure that required fields are present
        $required = array(
            'name',
            'school',
            'email',
            'admin',
            'status',
            'gender',
            'dob',
            'doc',
            'password'
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
        $details['password'] = password_hash($details['password'], PASSWORD_DEFAULT);

        $db = (new MongoDB\Client(connect_string))->selectDatabase('elmportal1');
        $collection = $db->selectCollection('tutors');
        $result = $collection->insertOne($details);

        return (string) $result->getInsertedId();
    }

    static function processTutorFilters(array $filters)
    {
        // filters: names, schools, statuses, genders, 
        // admins, dob range, date of commencement range, classes
        $result = array();
        if (isset($filters['name'])) {
            $result['name'] = array('$in' => array_map(function (string $filterString) {
                return new \MongoDB\BSON\Regex(preg_quote($filterString), 'i');
            }, $filters['name']));
        }
        if (isset($filters['school'])) {
            $result['school'] = array('$in' => array_map(function (string $filterString) {
                return new \MongoDB\BSON\Regex(preg_quote($filterString), 'i');
            }, $filters['school']));
        }
        if (isset($filters['gender'])) {
            $result['gender'] = array('$in' => $filters['gender']);
        }
        if (isset($filters['admin'])) {
            $result['admin'] = array('$in' => array_map('intval', $filters['admin']));
        }
        if (isset($filters['status'])) {
            $result['status'] = array('$in' => array_map('intval', $filters['status']));
        }
        if (isset($filters['dobLower']) && isset($filters['dobUpper'])) {
            $result['dob'] = array(
                '$gte' => new MongoDB\BSON\UTCDateTime(strtotime($filters['dobLower'][0]) * 1000),
                '$lte' => new MongoDB\BSON\UTCDateTime(strtotime($filters['dobUpper'][0]) * 1000)
            );
        } else if (isset($filters['dobLower'])) {
            $result['dob'] = array(
                '$gte' => new MongoDB\BSON\UTCDateTime(strtotime($filters['dobLower'][0]) * 1000)
            );
        } else if (isset($filters['dobUpper'])) {
            $result['dob'] = array(
                '$lte' => new MongoDB\BSON\UTCDateTime(strtotime($filters['dobUpper'][0]) * 1000)
            );
        }
        if (isset($filters['joinLower']) && isset($filters['joinUpper'])) {
            $result['doc'] = array(
                '$gte' => new MongoDB\BSON\UTCDateTime(strtotime($filters['joinLower'][0]) * 1000),
                '$lte' => new MongoDB\BSON\UTCDateTime(strtotime($filters['joinUpper'][0]) * 1000)
            );
        } else if (isset($filters['joinLower'])) {
            $result['doc'] = array(
                '$gte' => new MongoDB\BSON\UTCDateTime(strtotime($filters['joinLower'][0]) * 1000)
            );
        } else if (isset($filters['joinUpper'])) {
            $result['doc'] = array(
                '$lte' => new MongoDB\BSON\UTCDateTime(strtotime($filters['joinUpper'][0]) * 1000)
            );
        }
        // TODO filter by classes
        return $result;
    }

    static function getTutor(string $id)
    {
        $db = (new MongoDB\Client(connect_string))->selectDatabase('elmportal1');
        $collection = $db->selectCollection('tutors');
        return $collection->findOne(
            array('_id' => new MongoDB\BSON\ObjectId($id)),
            array('projection' => array(
                'password' => 0
            ))
        );
    }

    static function isValidTutor($id)
    {
        $db = (new MongoDB\Client(connect_string))->selectDatabase('elmportal1');
        $collection = $db->selectCollection('tutors');
        return $collection->countDocuments(array('_id' => new \MongoDB\BSON\ObjectId($id))) == 1;
    }

    static function getClasses(string $tutorId)
    {
        $db = (new MongoDB\Client(connect_string))->selectDatabase('elmportal1');
        $collection = $db->selectCollection('classes');
        $results = $collection->find(
            array(
                'tutors' => array(
                    '$elemMatch' => array(
                        'id' => new \MongoDB\BSON\ObjectId($tutorId)
                    )
                )
            ),
            array('projection' => array(
                'sessions' => 0,
                'tutors' => 0
            ))
        )->toArray();
        return $results;
    }

    static function getTutorList(int $page, array $filters, int $perPage = 20)
    {
        $db = (new MongoDB\Client(connect_string))->selectDatabase('elmportal1');
        $filterBy = DBTutor::processTutorFilters($filters);
        $collection = $db->selectCollection('tutors');
        $numResults = $collection->countDocuments($filterBy);
        $result = $collection->find(
            $filterBy,
            array(
                'projection' => array(
                    'password' => 0
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
}
