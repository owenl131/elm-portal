<?php

require __DIR__ . '/../../vendor/autoload.php';
require_once __DIR__ . '/../config.php';

class MClass
{
    public static array $cache = [];

    public string $id; // generated by MongoDB
    public string $name;
    public int $year;
    public array $days; // array of integers
    public string $timeslot;
    public float $duration;
    public bool $active;

    public ?array $sessions;

    public MongoDB\Database $db;

    /**
     * Constructor for class
     */
    function __construct(\MongoDB\Database $db, array $data)
    {
        MClass::validateFieldsPresent($data);
        $this->db = $db;
        $this->id = (string) $data['id'];
        $this->name = (string) $data['name'];
        $this->year = intval($data['year']);
        $this->days = array_unique(array_map(function ($x) {
            return intval($x);
        }, array_values($data['days'])), SORT_NUMERIC);
        sort($this->days);
        $this->timeslot = (string) $data['timeslot'];
        $this->duration = floatval($data['duration']);
        $this->active = boolval($data['active']);
    }

    /**
     * Retrieves class data by id
     */
    static function retrieve(MongoDB\Database $db, string $id): MClass
    {
        if (isset(MClass::$cache[$id])) {
            return MClass::$cache[$id];
        }

        $collection = $db->selectCollection('classes');
        $data = $collection->findOne(
            ['_id' => new MongoDB\BSON\ObjectId($id)],
            ['typeMap' => [
                'root' => 'array',
                'document' => 'array',
                'array' => 'array'
            ]]
        );
        if (is_null($data)) {
            throw new Exception("Invalid class id");
        }
        $data['id'] = (string) $data['_id'];
        $result = new MClass($db, $data);
        MClass::$cache[$id] = $result;
        return $result;
    }

    /**
     * Retrieves an array of classes, with filters
     */
    static function retrieveMany(MongoDB\Database $db, int $page, array $filters, int $perPage = 20): array
    {
        $collection = $db->selectCollection('classes');
        $filterBy = MClass::processFilters($filters);
        $numResults = $collection->countDocuments($filterBy);
        $result = $collection->find(
            $filterBy,
            [
                'projection' => [
                    'sessions' => 0,
                    'tutors' => 0
                ],
                'skip' => $page * $perPage,
                'limit' => $perPage,
                'typeMap' => [
                    'root' => 'array',
                    'document' => 'array',
                    'array' => 'array'
                ]
            ]
        )->toArray();
        $classList = array_map(function ($class) use ($db) {
            $class['id'] = (string) $class['_id'];
            unset($class['_id']);
            return new MClass($db, $class);
        }, $result);

        foreach ($classList as $c) {
            MClass::$cache[$c->id] = $c;
        }

        return array(
            'data' => $classList,
            'total' => $numResults,
            'perPage' => $perPage,
            'page' => $page
        );
    }

    /**
     * Throws an exception if any fields are not provided
     */
    static function validateFieldsPresent(array $data): void
    {
        if (!isset($data['id']))
            throw new Exception("Id not provided");
        if (!isset($data['name']))
            throw new Exception("Name not provided");
        if (!isset($data['year']))
            throw new Exception("Year not provided");
        if (!isset($data['timeslot']))
            throw new Exception("Timeslot not provided");
        if (!isset($data['duration']))
            throw new Exception("Duration not provided");
        if (!isset($data['active']))
            throw new Exception("Active not provided");
    }

    /**
     * Inserts a new class into the database
     */
    static function create(MongoDB\Database $db, array $data): MClass
    {
        $data['duration'] = $data['duration'] ?? 3;
        $data['year'] = $data['year'] ?? intval(date("Y"));
        $data['timeslot'] = $data['timeslot'] ?? "";
        $data['active'] = $data['active'] ?? true;
        $data['days'] = $data['days'] ?? [];
        MClass::validateFieldsPresent($data);
        $data['days'] = array_unique(array_map(function ($x) {
            return intval($x);
        }, array_values($data['days'])), SORT_NUMERIC);
        sort($data['days']);
        $details = [
            'name' => (string) $data['name'],
            'year' => intval($data['year']),
            'days' => $data['days'],
            'timeslot' => (string) $data['timeslot'],
            'duration' => floatval($data['duration']),
            'active' => boolval($data['active']),
            'sessions' => [],
            'present' => [],
            'absent' => [],
        ];
        if ($details['year'] < 2000 || $details['year'] > 2100)
            throw new Exception("Invalid year");
        $collection = $db->selectCollection('classes');
        $result = $collection->insertOne($details);
        $id = (string) $result->getInsertedId();
        return MClass::retrieve($db, $id);
    }

    /**
     * Updates fields of a class
     */
    function update($data): bool
    {
        $update = [];
        if (isset($data['name']))
            $update['name'] = (string) $data['name'];
        if (isset($data['year']))
            $update['year'] = intval($data['year']);
        if (isset($data['timeslot']))
            $update['timeslot'] = (string) $data['timeslot'];
        if (isset($data['duration']))
            $update['duration'] = floatval($data['duration']);
        if (isset($data['days'])) {
            $data['days'] = array_unique(array_map(function ($x) {
                return intval($x);
            }, array_values($data['days'])), SORT_NUMERIC);
            $update['days'] = $data['days'];
        }
        if (isset($data['active']))
            $update['active'] = boolval($data['active']);
        $collection = $this->db->selectCollection('classes');
        $result = $collection->updateOne(
            array('_id' => new \MongoDB\BSON\ObjectId($this->id)),
            array('$set' => $update)
        );
        return $result->isAcknowledged();
    }

    /**
     * Deletes a class from the database
     */
    function delete(): bool
    {
        // delete each session
        $sessions = $this->getSessions();
        foreach ($sessions as $sess) {
            $result = $sess->delete();
            if (!$result) {
                return false;
            }
        }
        // delete class data
        $collection = $this->db->selectCollection('classes');
        $result = $collection->deleteOne(
            ['_id' => new \MongoDB\BSON\ObjectId($this->id)]
        );
        return $result->isAcknowledged();
    }

    /**
     * Converts to an associative array, to be output in 
     * the HTTP response
     */
    function toAssoc(): array
    {
        return [
            'id' => $this->id,
            'name' => $this->name,
            'year' => $this->year,
            'days' => $this->days,
            'timeslot' => $this->timeslot,
            'duration' => $this->duration,
            'active' => $this->active,
        ];
    }

    /**
     * Processes filters to pass to MongoDB
     * Filters are: 
     * [
     *      name : string
     *      days : array of int
     *      yearLower : int
     *      yearUpper : int
     * ]
     */
    static function processFilters(array $filters)
    {
        $result = [];
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

    /**
     * @return MClassTutor[]
     */
    function getTutors(): array
    {
        $collection = $this->db->selectCollection('classes');
        $data = $collection->findOne(
            ['_id' => new MongoDB\BSON\ObjectId($this->id)],
            ['projection' => [
                'sessions' => 0
            ]]
        );
        if (is_null($data) || !isset($data['tutors'])) {
            return [];
        }
        $tutorList = iterator_to_array($data['tutors']);
        $tutorList = array_map(function ($t) {
            $tutor = MTutor::retrieve($this->db, (string) $t['id']);
            if (isset($t['joinedOn'])) {
                $joinDate = $t['joinedOn']->toDateTime();
            } else {
                $joinDate = null;
            }
            if (isset($t['leftOn'])) {
                $leaveDate = $t['leftOn']->toDateTime();
            } else {
                $leaveDate = null;
            }
            return new MClassTutor($this->db, $tutor, $this, $joinDate, $leaveDate);
        }, $tutorList);
        return $tutorList;
    }

    function hasTutor(MTutor $tutor): bool
    {
        foreach ($this->getTutors() as $t) {
            if ($t->tutor->id == $tutor->id) {
                return true;
            }
        }
        return false;
    }

    function addTutor(MTutor $tutor, DateTime $joinDate): bool
    {
        if ($this->hasTutor($tutor)) {
            return false;
        }
        $collection = $this->db->selectCollection('classes');
        $result = $collection->updateOne(
            array('_id' => new MongoDB\BSON\ObjectId($this->id)),
            array('$addToSet' => array('tutors' => array(
                'id' => new \MongoDB\BSON\ObjectId($tutor->id),
                'joinedOn' => new \MongoDB\BSON\UTCDateTime($joinDate->getTimestamp() * 1000)
            )))
        );
        return $result->isAcknowledged();
    }

    function removeTutor(MTutor $tutor): bool
    {
        if (!$this->hasTutor($tutor)) {
            return false;
        }
        $sessions = $this->getSessions();
        foreach ($sessions as $session) {
            if ($session->hasTutor($tutor))
                $session->markExempt($tutor);
        }
        $collection = $this->db->selectCollection('classes');
        $collection->updateOne(
            ['_id' => new MongoDB\BSON\ObjectId($this->id)],
            ['$pull' => ['tutors' => ['id' => new \MongoDB\BSON\ObjectId($tutor->id)]]]
        );
        return true;
    }

    /**
     * @return MClassSession[]
     */
    function getSessions(): array
    {
        // if result is cached
        if (isset($this->sessions)) {
            return $this->sessions;
        }
        $collection = $this->db->selectCollection('classes');
        $data = $collection->findOne(
            array('_id' => new MongoDB\BSON\ObjectId($this->id)),
            array('projection' => array(
                'sessions' => 1
            ))
        );
        if (is_null($data) || !isset($data['sessions'])) {
            return array();
        }
        $sessList = iterator_to_array($data['sessions']);
        $sessList = array_map(function ($s) {
            return new MClassSession(
                $this->db,
                $this,
                (string) $s['_id'],
                $s['date']->toDateTime(),
                $s['remarks'] ?? "",
                $s['duration'] ?? 3
            );
        }, $sessList);
        // cache results 
        $this->sessions = $sessList;
        return $sessList;
    }

    function getSession(string $sessionId): ?MClassSession
    {
        $sessions = $this->getSessions();
        $sessions = array_values(array_filter($sessions, function (MClassSession $elem) use ($sessionId) {
            return $elem->id == $sessionId;
        }));
        if (count($sessions) == 0) {
            return null;
        }
        return $sessions[0];
    }

    /**
     * Add a class session
     */
    function addSession($data): MClassSession
    {
        if (!isset($data['date']))
            throw new Exception("Date not provided");
        if (!isset($data['remarks']))
            throw new Exception("Remarks not provided");

        $body = array();
        $body['remarks'] = (string) $data['remarks'];
        $body['date'] = new MongoDB\BSON\UTCDateTime(strtotime($data['date']) * 1000);
        $body['duration'] = floatval($data['duration'] ?? 3);

        $collection = $this->db->selectCollection('classes');
        $body['_id'] = new \MongoDB\BSON\ObjectId();

        $result = $collection->updateOne(
            array('_id' => new \MongoDB\BSON\ObjectId($this->id)),
            array('$push' => array('sessions' => $body))
        );
        $this->sessions = null;
        if ($result->isAcknowledged()) {
            return new MClassSession(
                $this->db,
                $this,
                (string) $body['_id'],
                $body['date']->toDateTime(),
                $body['remarks'],
                $body['duration']
            );
        }
        return null;
    }
}
