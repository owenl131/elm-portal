<?php

require __DIR__ . '/../../vendor/autoload.php';
require_once __DIR__ . '/../config.php';

class MTutor
{
    public static array $cache = [];

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
    public ?DateTime $sessionExpiry;
    public ?string $sessionId;

    public array $languages;
    public array $available;
    public array $subjects;
    public ?string $careerGoal;
    public ?string $schoolType;
    public ?int $yearOfGraduation;
    public array $remarks;

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
        $this->doc = $data['doc'];
        $this->password = (string) $data['password'];
        $this->sessionExpiry = $data['sessionExpiry'] ?? null;
        $this->sessionId = $data['sessionId'] ?? null;
        // extended fields
        $this->languages = $data['languages'] ?? [];
        $this->available = $data['available'] ?? [];
        $this->subjects = $data['subjects'] ?? [];
        $this->careerGoal = $data['careerGoal'] ?? "";
        $this->schoolType = $data['schoolType'] ?? "none";
        $this->yearOfGraduation = $data['yearOfGraduation'] ?? null;
        $this->remarks = $data['remarks'] ?? [];
    }

    /**
     * @return MTutor|null
     */
    static function retrieve(MongoDB\Database $db, string $id): ?MTutor
    {
        // check if object has been retrieved before
        if (isset(MTutor::$cache[$id])) {
            return MTutor::$cache[$id];
        }
        // pull data from tutors collection
        $collection = $db->selectCollection('tutors');
        $data = $collection->findOne(
            ['_id' => new MongoDB\BSON\ObjectId($id)],
            ['typeMap' => [
                'root' => 'array',
                'document' => 'array',
                'array' => 'array'
            ]]
        );
        if (is_null($data))
            return null;
        // handle parsing from MongoDB
        $data['id'] = (string) $data['_id'];
        if (isset($data['dob']))
            $data['dob'] = $data['dob']->toDateTime();
        if (isset($data['doc']))
            $data['doc'] = $data['doc']->toDateTime();
        if (isset($data['sessionExpiry']))
            $data['sessionExpiry'] = $data['sessionExpiry']->toDateTime();
        // return result
        $result = new MTutor($db, $data);
        MTutor::$cache[$id] = $result;
        return $result;
    }

    static function retrieveMany(MongoDB\Database $db, int $page, array $filters, int $perPage = 20): array
    {
        // convert filter array to be used by MongoDB
        $filterBy = MTutor::processTutorFilters($filters);
        $collection = $db->selectCollection('tutors');
        // report total number of records
        $numResults = $collection->countDocuments($filterBy);
        $result = $collection->find(
            $filterBy,
            [
                'skip' => $page * $perPage,
                'limit' => $perPage,
                'typeMap' => [
                    'root' => 'array',
                    'document' => 'array',
                    'array' => 'array'
                ]
            ]
        )->toArray();
        // handle parsing from MongoDB
        $tutors = array_map(function ($data) use ($db) {
            $data['id'] = (string) $data['_id'];
            if (isset($data['dob']))
                $data['dob'] = $data['dob']->toDateTime();
            if (isset($data['doc']))
                $data['doc'] = $data['doc']->toDateTime();
            if (isset($data['sessionExpiry']))
                $data['sessionExpiry'] = $data['sessionExpiry']->toDateTime();
            return new MTutor($db, $data);
        }, $result);
        // save in cache
        foreach ($tutors as $t) {
            MTutor::$cache[$t->id] = $t;
        }
        // return results
        return array(
            'data' => $tutors,
            'total' => $numResults,
            'perPage' => $perPage,
            'page' => $page
        );
    }

    /**
     * @return MTutor|null
     */
    static function retrieveBySessionId(MongoDB\Database $db, string $sessionId): ?MTutor
    {
        // check if it is available in cache
        foreach (MTutor::$cache as $id => $t) {
            if ($t->sessionId == $sessionId) {
                return $t;
            }
        }
        $collection = $db->selectCollection('tutors');
        $result = $collection->countDocuments([
            'sessionId' => new \MongoDB\BSON\ObjectId($sessionId),
            'sessionExpiry' => ['$exists' => 1, '$gte' => new \MongoDB\BSON\UTCDateTime()]
        ]);
        // extract where sessionId matches and not expired
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

    /**
     * @return MTutor|null
     */
    static function retrieveByEmail(MongoDB\Database $db, string $email): ?MTutor
    {
        // check if available in cache
        foreach (MTutor::$cache as $id => $t) {
            if ($t->email == $email) {
                return $t;
            }
        }
        // pull from database
        $collection = $db->selectCollection('tutors');
        if ($collection->countDocuments(['email' => $email]) == 1) {
            $data = $collection->findOne(
                ['email' => $email],
                ['projection' => [
                    '_id' => 1,
                    'email' => 1
                ]]
            );
            if (is_null($data)) {
                return null;
            }
            return MTutor::retrieve($db, (string) $data['_id']);
        }
        return null;
    }

    static function retrieveByCredentials(MongoDB\Database $db, string $email, string $password): ?MTutor
    {
        $collection = $db->selectCollection('tutors');
        // check if email exists
        $tutor = MTutor::retrieveByEmail($db, $email);
        if (is_null($tutor)) {
            return null;
        }

        $isValid = password_verify($password, $tutor->password);
        if ($isValid) {
            if (!isset($tutor->sessionExpiry) || $tutor->sessionExpiry < new DateTime()) {
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
            unset(MTutor::$cache[$tutor->id]);
            return MTutor::retrieve($db, $tutor->id);
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
        if (!filter_var($data['email'], FILTER_VALIDATE_EMAIL))
            throw new Exception("Tutor email is malformed");
        if (strlen($data['name']) == 0)
            throw new Exception("Tutor email must not be empty");
        if (!isset($data['status']))
            throw new Exception("Tutor status not set");
        if (!is_numeric($data['status']))
            throw new Exception("Tutor status must be an integer");
        if ($data['status'] < 0 || $data['status'] > 2)
            throw new Exception("Tutor status is out of range");
        if (!isset($data['admin']))
            throw new Exception("Tutor admin level not set");
        if (!is_numeric($data['admin']))
            throw new Exception("Tutor admin level must be an integer");
        if ($data['admin'] < 0 || $data['admin'] > 1)
            throw new Exception("Tutor admin level is out of range");
        if (!isset($data['gender']))
            throw new Exception("Tutor gender not set");
        if ($data['gender'] != 'm' && $data['gender'] != 'f')
            throw new Exception("Tutor gender is invalid");
        if (!isset($data['dob']))
            throw new Exception("Tutor date of birth not set");
        if (!$data['dob'] instanceof DateTime)
            throw new Exception("Tutor date of birth invalid");
        if (!isset($data['doc']))
            throw new Exception("Tutor date of commencement not set");
        if (!$data['doc'] instanceof DateTime)
            throw new Exception("Tutor date of registration invalid");
        if (!isset($data['password']))
            throw new Exception("Tutor password not provided");
        if (strlen($data['password']) == 0)
            throw new Exception("Tutor password must not be empty");
        if (strlen($data['password']) < 8)
            throw new Exception("Tutor password must be at least 8 characters long");
    }

    static function create(MongoDB\Database $db, array $data): MTutor
    {
        if (isset($data['dob'])) $data['dob'] = new DateTime($data['dob']);
        if (isset($data['doc'])) $data['doc'] = new DateTime($data['doc']);
        // ensure email has not duplicates
        MTutor::validateFieldsPresent($data);
        $data['school'] = $data['school'] ?? "";
        if (!is_null(MTutor::retrieveByEmail($db, $data['email']))) {
            throw new Exception("Email already exists");
        }
        $details = [
            'name' => (string) $data['name'],
            'email' => (string) $data['email'],
            'status' => intval($data['status']),
            'admin' => intval($data['admin']),
            'password' => password_hash($data['password'], PASSWORD_DEFAULT),
            'gender' => (string) $data['gender'],
            'school' => (string) $data['school'],
            'dob' => new MongoDB\BSON\UTCDateTime($data['dob']->getTimestamp() * 1000),
            'doc' => new MongoDB\BSON\UTCDateTime($data['doc']->getTimestamp() * 1000)
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
        $update = [];
        // ensure email has no duplicates
        if (isset($data['name']) && $data['name'] != $this->name && strlen($data['name']) != 0) {
            $update['name'] = (string) $data['name'];
        }
        if (
            isset($data['email'])
            && $data['email'] != $this->email
            && strlen($data['email']) != 0
            && filter_var($data['email'], FILTER_VALIDATE_EMAIL)
            && is_null(MTutor::retrieveByEmail($this->db, $data['email']))
        ) {
            $update['email'] = (string) $data['email'];
        }
        if (
            isset($data['admin'])
            && is_int($data['admin'])
            && $data['admin'] != $this->adminLvl
            && ($data['admin'] >= 0 && $data['admin'] <= 1)
        ) {
            $update['admin'] = $data['admin'];
        }
        if (
            isset($data['status'])
            && is_int($data['status'])
            && $data['status'] != $this->status
            && ($data['status'] >= 0 && $data['status'] <= 2)
        ) {
            $update['status'] = $data['status'];
        }
        if (
            isset($data['gender'])
            && ($data['gender'] == 'm' || $data['gender'] == 'f')
            && $data['gender'] != $this->gender
        ) {
            $update['gender'] = (string) $data['gender'];
        }
        if (isset($data['password'])) {
            if (strlen($data['password']) < 8) {
                throw new Exception("Password must have at least 8 characters");
            }
            $update['password'] = password_hash($data['password'], PASSWORD_DEFAULT);
        }
        if (isset($data['school']) && strlen($data['school']) != 0 && $data['school'] != $this->school) {
            $update['school'] = (string) $data['school'];
        }
        if (isset($data['dob']) && $this->dob->format('Y-m-d') != $data['dob'] && strtotime($data['dob'])) {
            $update['dob'] = new MongoDB\BSON\UTCDateTime(strtotime($data['dob']) * 1000);
        }
        if (isset($data['doc']) && $this->doc->format('Y-m-d') != $data['doc'] && strtotime($data['doc'])) {
            $update['doc'] = new MongoDB\BSON\UTCDateTime(strtotime($data['doc']) * 1000);
        }
        $collection = $this->db->selectCollection('tutors');
        $result = $collection->updateOne(
            array('_id' => new \MongoDB\BSON\ObjectId($this->id)),
            array('$set' => $update)
        );

        unset(MTutor::$cache[$this->id]);
        return $result->isAcknowledged();
    }

    function delete(): bool
    {
        // delete attendance records
        $classes = $this->getClasses();
        foreach ($classes as $class) {
            $class->removeTutor($this);
        }
        // delete from database
        $collection = $this->db->selectCollection('tutors');
        $result = $collection->deleteOne([
            '_id' => new MongoDB\BSON\ObjectId($this->id)
        ]);
        return $result->isAcknowledged();
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
            'dob' => $this->dob->format('Y-m-d'),
            'doc' => $this->doc->format('Y-m-d'),
        ];
        return $result;
    }

    function toExtendedAssoc(): array
    {
        $result = [
            'languages' => $this->languages,
            'available' => $this->available,
            'subjects' => $this->subjects,
            'careerGoal' => $this->careerGoal,
            'schoolType' => $this->schoolType,
            'remarks' => $this->remarks
        ];
        if (isset($this->yearOfGraduation))
            $result['yearOfGraduation'] = $this->yearOfGraduation;
        return $result;
    }

    /**
     * @return MClass[]
     */
    function getClasses(): array
    {
        $collection = $this->db->selectCollection('classes');
        $results = $collection->find(
            ['tutors' => [
                '$elemMatch' => [
                    'id' => new \MongoDB\BSON\ObjectId($this->id)
                ]
            ]],
            ['projection' => [
                'sessions' => 0,
                'tutors' => 0
            ]]
        )->toArray();
        $results = array_values($results);
        $results = array_map(function ($elem) {
            return MClass::retrieve($this->db, (string) $elem['_id']);
        }, $results);
        return $results;
    }

    /**
     * @return MClassSession[]
     */
    function getClassSessions(MClass $class)
    {
        $sessions = $class->getSessions();
        $sessions = array_filter($sessions, function (MClassSession $elem) {
            return $elem->hasTutor($this) && !$elem->isTutorExempt($this);
        });
        return $sessions;
    }

    /**
     * @return float
     */
    function getClassHours(MClass $class): float
    {
        $sessions = $this->getClassSessions($class);
        $sessions = array_filter($sessions, function (MClassSession $elem) {
            return $elem->isTutorPresent($this);
        });
        $hours = array_map(function (MClassSession $elem) {
            return $elem->duration;
        }, $sessions);
        $totalHours = array_sum($hours);
        return $totalHours;
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
        if (isset($filters['excludeIds'])) {
            if (!isset($result['_id'])) {
                $result['_id'] = array();
            }
            $result['_id']['$nin'] = $filters['excludeIds'];
        }
        // TODO filter by classes
        return $result;
    }
}
