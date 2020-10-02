<?php

require '../../vendor/autoload.php';
require '../config.php';

class DBTutor
{
    static function authenticateBySessionId(string $sessionId)
    {
        $db = new MongoDB\Client(connect_string);
        $collection = $db->tutors;
        $result = $collection->find(array(
            'sessionId' => $sessionId,
            'sessionExpiry' => array('$lte', new MongoDB\BSON\UTCDateTime(time() * 1000))
        ));
        if ($result->count() == 1) {
            return true;
        } else {
            return false;
        }
    }

    static function authenticateByCredentials(string $email, string $password)
    {
        $db = new MongoDB\Client(connect_string);
        $collection = $db->tutors;
        $result = $collection->find(
            array('email' => $email),
            array('projection' => array('password' => 1))
        );
        if ($result->count() == 1) {
            $element = $result->getNext();
            $isValid = password_verify($password, $element['password']);
            if ($isValid) {
                $collection->update(
                    array('_id' => $element['_id']),
                    array(
                        'sessionId' => new \MongoDB\BSON\ObjectId(),
                        'sessionExpiry' => new \MongoDB\BSON\UTCDateTime(time() + 3600)
                    )
                );
                $result = $collection->find(
                    array('_id' => $element['_id']),
                    array('projection' => array('sessionId' => 1))
                );
                $element = $result->getNext();
                return $element['sessionId'];
            }
        }
        return false;
    }

    function addTutor(array $details)
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
        $details = array_column(array($details), $required)[0];
        $details['password'] = password_hash($details['password'], PASSWORD_DEFAULT);

        $db = new MongoDB\Client(connect_string);
        $collection = $db->tutors;
        $collection->insert($details);

        return $details['_id'];
    }

    static function processTutorFilters(array $filters)
    {
        // filters: names, schools, statuses, genders, 
        // admins, dob range, date of commencement range, classes
        $result = array();
        if (isset($filters['name'])) {
            $result['name'] = array('$in' => array_map(function (string $filterString) {
                return '/' . preg_quote($filterString) . '/i';
            }, $filters['name']));
        }
        if (isset($filters['school'])) {
            $result['school'] = array('$in' => array_map(function (string $filterString) {
                return '/' . preg_quote($filterString) . '/i';
            }, $filters['school']));
        }
        if (isset($filters['gender'])) {
            $result['gender'] = array('$in' => $filters['gender']);
        }
        if (isset($filters['admin'])) {
            $result['adminLevel'] = array('$in' => $filters['admin']);
        }
        if (isset($filters['status'])) {
            $result['status'] = array('$in' => $filters['status']);
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
            $result['joinDate'] = array(
                '$gte' => new MongoDB\BSON\UTCDateTime(strtotime($filters['joinLower'][0]) * 1000),
                '$lte' => new MongoDB\BSON\UTCDateTime(strtotime($filters['joinUpper'][0]) * 1000)
            );
        } else if (isset($filters['joinLower'])) {
            $result['joinDate'] = array(
                '$gte' => new MongoDB\BSON\UTCDateTime(strtotime($filters['joinLower'][0]) * 1000)
            );
        } else if (isset($filters['joinUpper'])) {
            $result['joinDate'] = array(
                '$lte' => new MongoDB\BSON\UTCDateTime(strtotime($filters['joinUpper'][0]) * 1000)
            );
        }
        // TODO filter by classes
        return $result;
    }

    static function getTutorList(int $page, array $filters, int $perPage = 20)
    {
        $numToSkip = $page * $perPage;
        $db = new MongoDB\Client(connect_string);
        $filterBy = DBTutor::processTutorFilters($filters);
        $collection = $db->tutors;
        $result = $collection->find(
            $filterBy,
            array('projection' => array(
                'password' => 0
            ))
        );
        $numResults = $result->count();
        $result->skip($numToSkip);
        $result->limit($perPage);
        return array(
            'data' => iterator_to_array($result),
            'total' => $numResults
        );
    }
}
