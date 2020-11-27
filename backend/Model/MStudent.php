<?php

class MStudent
{
    public static array $cache = [];

    public MongoDB\Database $db;

    public string $id;
    public string $name;
    public DateTime $dob;
    public DateTime $doc;
    public string $gender;
    public int $status;

    function __construct(MongoDB\Database $db, array $data)
    {
        if (!isset($data['id']))
            throw new Exception("Student id not provided");
        $this->db = $db;
        $this->id = (string) $data['id'];
        $this->name = (string) $data['name'];
        $this->dob = $data['dob'];
        $this->doc = $data['doc'];
        $this->gender = (string) $data['gender'];
        $this->status = intval($data['status']);
    }

    static function validateFieldsPresent(array $data): void
    {
        if (!isset($data['name']))
            throw new Exception("Student name not provided");
        if (!isset($data['gender']))
            throw new Exception("Student gender not set");
        if ($data['status'] < 0 || $data['status'] > 2)
            throw new Exception("Student status is out of range");
        if ($data['gender'] != 'm' && $data['gender'] != 'f')
            throw new Exception("Student gender is invalid");
        if (!isset($data['dob']))
            throw new Exception("Student date of birth not set");
        if (!$data['dob'] instanceof DateTime)
            throw new Exception("Student date of birth invalid");
        if (!isset($data['doc']))
            throw new Exception("Student date of commencement not set");
        if (!$data['doc'] instanceof DateTime)
            throw new Exception("Student date of commencement invalid");
    }

    // TODO
    static function retrieve(MongoDB\Database $db, string $id): ?MStudent
    {
        if (isset(MStudent::$cache[$id])) {
            return MStudent::$cache[$id];
        }
        $collection = $db->selectCollection('students');
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
        $result = new MStudent($db, $data);
        MStudent::$cache[$id] = $result;
        return $result;
    }

    static function retrieveMany(MongoDB\Database $db, int $page, array $filters, int $perPage = 20): array
    {
        // TODO handle filters
        $filterBy = [];
        $collection = $db->selectCollection('students');
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
        $students = array_map(function ($data) use ($db) {
            $data['id'] = (string) $data['_id'];
            if (isset($data['dob']))
                $data['dob'] = $data['dob']->toDateTime();
            if (isset($data['doc']))
                $data['doc'] = $data['doc']->toDateTime();
            return new MStudent($db, $data);
        }, $result);
        // save in cache
        foreach ($students as $s) {
            MStudent::$cache[$s->id] = $s;
        }
        // return results
        return array(
            'data' => $students,
            'total' => $numResults,
            'perPage' => $perPage,
            'page' => $page
        );
    }

    static function create(MongoDB\Database $db, array $data)
    {
        if (isset($data['dob'])) $data['dob'] = new DateTime($data['dob']);
        if (isset($data['doc'])) $data['doc'] = new DateTime($data['doc']);
        MStudent::validateFieldsPresent($data);
        $details = [
            'name' => (string) $data['name'],
            'status' => intval($data['status']),
            'gender' => (string) $data['gender'],
            'dob' => new MongoDB\BSON\UTCDateTime($data['dob']->getTimestamp() * 1000),
            'doc' => new MongoDB\BSON\UTCDateTime($data['doc']->getTimestamp() * 1000)
        ];
        $collection = $db->selectCollection('students');
        $result = $collection->insertOne($details);
        if ($result->isAcknowledged()) {
            $id = (string) $result->getInsertedId();
            return MStudent::retrieve($db, $id);
        }
        throw new Exception("Failed to add student");
    }

    function update(array $data): bool
    {
        $update = [];
        if (isset($data['name']) && $data['name'] != $this->name && strlen($data['name']) != 0) {
            $update['name'] = (string) $data['name'];
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
        if (isset($data['dob']) && $this->dob->format('Y-m-d') != $data['dob'] && strtotime($data['dob'])) {
            $update['dob'] = new MongoDB\BSON\UTCDateTime(strtotime($data['dob']) * 1000);
        }
        if (isset($data['doc']) && $this->doc->format('Y-m-d') != $data['doc'] && strtotime($data['doc'])) {
            $update['doc'] = new MongoDB\BSON\UTCDateTime(strtotime($data['doc']) * 1000);
        }
        $collection = $this->db->selectCollection('students');
        $result = $collection->updateOne(
            array('_id' => new \MongoDB\BSON\ObjectId($this->id)),
            array('$set' => $update)
        );

        unset(MStudent::$cache[$this->id]);
        return $result->isAcknowledged();
    }

    function delete(): bool
    {
        // delete attendance records
        // $classes = $this->getClasses();
        // foreach ($classes as $class) {
        //     $class->removeStudent($this);
        // }
        // delete from database
        $collection = $this->db->selectCollection('students');
        $result = $collection->deleteOne([
            '_id' => new MongoDB\BSON\ObjectId($this->id)
        ]);
        return $result->isAcknowledged();
    }

    function toAssoc(): array
    {
        return [
            'id' => $this->id,
            'name' => $this->name,
            'status' => $this->status,
            'gender' => $this->gender,
            'dob' => $this->dob->format('Y-m-d'),
            'doc' => $this->doc->format('Y-m-d'),
            'schools' => [],
        ];
    }
}
