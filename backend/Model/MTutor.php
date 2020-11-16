<?php

require __DIR__ . '/../../vendor/autoload.php';
require_once __DIR__ . '/../config.php';

class MTutor
{
    public string $id;
    public string $name;
    public string $school;
    public string $email;
    public int $adminLvl;
    public int $status;
    public string $gender;
    public DateTime $dob;
    public DateTime $doc;
    public string $password;
    public DateTime $sessionExpiry;
    public string $sessionId;

    public MongoDB\Database $db;

    function __construct(MongoDB\Database $db, array $data)
    {
        if (!isset($data['id']))
            throw new Exception("Tutor id not provided");
        MTutor::validateFieldsPresent($data);
        $this->db = $db;
        $this->id = (string) $data['id'];
        $this->name = (string) $data['name'];
        $this->school = (string) ($data['school'] ?? "");
        $this->email = (string) $data['email'];
        $this->adminLvl = intval($data['admin']);
        $this->status = intval($data['status']);
        $this->gender = (string) $data['gender'];
        $this->dob = $data['dob'];
        $this->doc = $data['dob'];
        $this->password = (string) $data['password'];
        $this->sessionExpiry = $data['sessionExpiry'];
        $this->sessionId = $data['sessionId'];
    }

    static function retrieve(MongoDB\Database $db, string $id): MTutor
    {
        $collection = $db->selectCollection('tutors');
        $data = $collection->findOne(
            array('_id' => new MongoDB\BSON\ObjectId($id)),
            array('projection' => array())
        );
        if (is_null($data))
            throw new Exception("Invalid tutor id");
        $data['id'] = (string) $data['_id'];
        if (isset($data['dob']))
            $data['dob'] = $data['dob']->toDateTime();
        if (isset($data['doc']))
            $data['doc'] = $data['doc']->toDateTime();
        return new MTutor($db, $data);
    }

    static function retrieveMany(MongoDB\Database $db, int $page, array $filters, int $perPage = 20): array
    {
        $filterBy = MTutor::processTutorFilters($filters);
        $collection = $db->selectCollection('tutors');
        $numResults = $collection->countDocuments($filterBy);
        $result = $collection->find(
            $filterBy,
            array(
                'skip' => $page * $perPage,
                'limit' => $perPage
            )
        )->toArray();
        $tutors = array_map(function ($data) use ($db) {
            $data['id'] = (string) $data['_id'];
            if (isset($data['dob']))
                $data['dob'] = $data['dob']->toDateTime();
            if (isset($data['doc']))
                $data['doc'] = $data['doc']->toDateTime();
            return new MTutor($db, $data);
        }, $result);
        return array(
            'data' => $tutors,
            'total' => $numResults,
            'perPage' => $perPage,
            'page' => $page
        );
    }

    static function retrieveBySessionId(MongoDB\Database $db, string $sessionId): MTutor
    {
        $collection = $db->selectCollection('tutors');
        $result = $collection->countDocuments([
            'sessionId' => new \MongoDB\BSON\ObjectId($sessionId),
            'sessionExpiry' => ['$exists' => 1, '$gte' => new \MongoDB\BSON\UTCDateTime()]
        ]);
        if ($result == 1) {
            $tutor = $collection->findOne(
                ['sessionId' => new \MongoDB\BSON\ObjectId($sessionId)],
                ['projection' => array('_id' => 1)]
            );
            return MTutor::retrieve($db, (string) $tutor['_id']);
        } else {
            return null;
        }
    }

    static function retrieveByEmail(MongoDB\Database $db, string $email): MTutor
    {
        $collection = $db->selectCollection('tutors');
        if ($collection->countDocuments(array('email' => $email)) == 1) {
            $data = $collection->findOne(
                array('email' => $email),
                array()
            );
            if (is_null($data))
                throw new Exception("Invalid tutor email");
            $data['id'] = (string) $data['_id'];
            if (isset($data['dob']))
                $data['dob'] = $data['dob']->toDateTime();
            if (isset($data['doc']))
                $data['doc'] = $data['doc']->toDateTime();
            return new MTutor($db, $data);
        }
        return null;
    }

    static function retrieveByCredentials(MongoDB\Database $db, string $email, string $password)
    {
        $collection = $db->selectCollection('tutors');
        // check if email exists
        if ($collection->countDocuments(array('email' => $email)) == 1) {
            $tutor = MTutor::retrieveByEmail($db, $email);
            if (is_null($tutor)) {
                throw new Exception("Tutor should not be null at this point");
            }
            $isValid = password_verify($password, $tutor->password);
            if ($isValid) {
                if (isset($tutor->sessionExpiry) || $tutor->sessionExpiry < new DateTime()) {
                    // either does not have any sessionId or sessionId has expired
                    $collection->updateOne(
                        ['_id' => new \MongoDB\BSON\ObjectId($tutor->id)],
                        ['$set' => [
                            'sessionId' => new \MongoDB\BSON\ObjectId(),
                            'sessionExpiry' => new \MongoDB\BSON\UTCDateTime((time() + 3600) * 1000)
                            // Session lasts for 1 hour
                        ]]
                    );
                }
                return MTutor::retrieve($db, $tutor->id);
            }
        }
        return null;
    }

    static function validateFieldsPresent(array $data): void
    {
        if (!isset($data['name']))
            throw new Exception("Tutor name not provided");
        if (strlen($data['name']) == 0)
            throw new Exception("Tutor name must not be empty");
        // school optional
        if (!isset($data['email']))
            throw new Exception("Tutor email not provided");
        if (strlen($data['name']) == 0)
            throw new Exception("Tutor email must not be empty");
        if (!isset($data['status']))
            throw new Exception("Tutor status not set");
        if (!is_int($data['status']))
            throw new Exception("Tutor status must be an integer");
        if ($data['status'] < 0 || $data['status'] > 2)
            throw new Exception("Tutor status is out of range");
        if (!isset($data['admin']))
            throw new Exception("Tutor admin level not set");
        if (!is_int($data['admin']))
            throw new Exception("Tutor admin level must be an integer");
        if ($data['admin'] < 0 || $data['admin'] > 1)
            throw new Exception("Tutor admin level is out of range");
        if (!isset($data['gender']))
            throw new Exception("Tutor gender not set");
        if ($data['gender'] != 'm' && $data['gender'] != 'f')
            throw new Exception("Tutor gender is invalid");
        if (!isset($data['dob']))
            throw new Exception("Tutor date of birth not set");
        if (!strtotime($data['dob']))
            throw new Exception("Tutor date of birth invalid");
        if (!isset($data['doc']))
            throw new Exception("Tutor date of commencement not set");
        if (!strtotime($data['doc']))
            throw new Exception("Tutor date of registration invalid");
        if (!isset($data['password']))
            throw new Exception("Tutor password not provided");
        if (strlen($data['password']) == 0)
            throw new Exception("Tutor password must not be empty");
    }

    static function create(MongoDB\Database $db, array $data): MTutor
    {
        // ensure email has not duplicates
        MTutor::validateFieldsPresent($data);
        $data['school'] = $data['school'] ?? "";
        $details = [
            'name' => (string) $data['name'],
            'email' => (string) $data['email'],
            'status' => intval($data['status']),
            'admin' => intval($data['admin']),
            'password' => password_hash($data['password'], PASSWORD_DEFAULT),
            'gender' => (string) $data['gender'],
            'school' => (string) $data['school'],
            'dob' => new MongoDB\BSON\UTCDateTime(strtotime($data['dob']) * 1000),
            'doc' => new MongoDB\BSON\UTCDateTime(strtotime($data['doc']) * 1000)
        ];
        $collection = $db->selectCollection('tutors');
        $result = $collection->insertOne($details);
        if ($result->isAcknowledged()) {
            $id = (string) $result->getInsertedId();
            return MTutor::retrieve($db, $id);
        }
        throw new Exception("Failed to add tutor");
    }

    function update($data): bool
    {
        // ensure email has no duplicates
    }

    function delete(): bool
    {
        // delete attendance records
        // delete from database
    }

    function isLeaderAbove(): bool
    {
        return $this->adminLvl <= 1;
    }

    function isAdminAbove(): bool
    {
        return $this->adminLvl == 0;
    }

    function toAssoc(): array
    {
        $result = [
            'id' => $this->id,
            'name' => $this->name,
            'school' => $this->school ?? "",
            'email' => $this->email,
            'admin' => $this->adminLvl,
            'status' => $this->status,
            'gender' => $this->gender,
            'dateOfBirth' => $this->dob->format('Y-m-d'),
            'dateOfRegistration' => $this->doc->format('Y-m-d'),
        ];
        return $result;
    }

    function getClasses()
    {
    }

    function getClassSessions()
    {
    }

    function getClassHours()
    {
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
}
