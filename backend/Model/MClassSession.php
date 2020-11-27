<?php

class MClassSession
{
    public MongoDB\Database $db;

    public MClass $class;
    public string $id;
    public DateTime $date;
    public string $remarks;
    public float $duration;

    function __construct(MongoDB\Database $db, MClass $class, string $id, DateTime $date, string $remarks, float $duration)
    {
        $this->db = $db;
        $this->class = $class;
        $this->id = $id;
        $this->date = $date;
        $this->remarks = $remarks;
        $this->duration = $duration;
    }

    /**
     * @return MClassTutor[]
     */
    function allClassTutors(): array
    {
        $tutors = $this->class->getTutors();
        $tutors = array_values(array_filter($tutors, function (MClassTutor $elem) {
            if ($this->date < $elem->joinDate) {
                return false;
            }
            if (!is_null($elem->leaveDate) && $this->date > $elem->leaveDate) {
                return false;
            }
            return true;
        }));
        return $tutors;
    }

    /**
     * @return MTutor[]
     */
    function allTutors(): array
    {
        $tutors = $this->class->getTutors();
        $tutors = array_filter($tutors, function (MClassTutor $elem) {
            if ($this->date < $elem->joinDate) {
                return false;
            }
            if (!is_null($elem->leaveDate) && $this->date > $elem->leaveDate) {
                return false;
            }
            return true;
        });
        $tutors = array_map(function (MClassTutor $elem) {
            return MTutor::retrieve($this->db, $elem->tutor->id);
        }, $tutors);
        return $tutors;
    }

    function hasTutor(MTutor $tutor): bool
    {
        $tutors = $this->allTutors();
        foreach ($tutors as $t) {
            if ($t->id == $tutor->id)
                return true;
        }
        return false;
    }

    /**
     * @return MTutor[]
     */
    function tutorsPresent(): array
    {
        $collection = $this->db->selectCollection('attendance');
        $session = $collection->findOne(
            ['_id' => new MongoDB\BSON\ObjectId($this->id)],
            ['typeMap' => [
                'root' => 'array',
                'document' => 'array',
                'array' => 'array'
            ]]
        );
        if (is_null($session))
            return [];
        if (!isset($session['attendance']))
            return [];
        $result = [];
        foreach ($session['attendance'] as $tutorId => $status) {
            if ($status == 'p') {
                array_push($result, (string) $tutorId);
            }
        }
        $result = array_values(array_map(function (string $id) {
            return MTutor::retrieve($this->db, $id);
        }, $result));
        return $result;
    }

    /**
     * @return bool
     */
    function isTutorPresent(MTutor $tutor): bool
    {
        $present = $this->tutorsPresent();
        return count(array_filter($present, function (MTutor $elem) use ($tutor) {
            return $elem->id == $tutor->id;
        })) == 1;
    }

    /**
     * @return bool
     */
    function isTutorAbsent(MTutor $tutor): bool
    {
        $absent = $this->tutorsAbsent();
        return count(array_filter($absent, function (MTutor $elem) use ($tutor) {
            return $elem->id == $tutor->id;
        })) == 1;
    }

    /**
     * @return bool
     */
    function isTutorExempt(MTutor $tutor): bool
    {
        $exempt = $this->tutorsExempt();
        return count(array_filter($exempt, function (MTutor $elem) use ($tutor) {
            return $elem->id == $tutor->id;
        })) == 1;
    }

    /**
     * @return MTutor[]
     */
    function tutorsAbsent(): array
    {
        $collection = $this->db->selectCollection('attendance');
        $session = $collection->findOne(
            ['_id' => new MongoDB\BSON\ObjectId($this->id)],
            ['typeMap' => [
                'root' => 'array',
                'document' => 'array',
                'array' => 'array'
            ]]
        );
        if (is_null($session))
            return [];
        if (!isset($session['attendance']))
            return [];
        $result = [];
        foreach ($session['attendance'] as $tutorId => $status) {
            if ($status == 'a') {
                array_push($result, (string) $tutorId);
            }
        }
        $result = array_values(array_map(function (string $id) {
            return MTutor::retrieve($this->db, $id);
        }, $result));
        return $result;
    }

    /**
     * @return MTutor[]
     */
    function tutorsExempt(): array
    {
        $collection = $this->db->selectCollection('attendance');
        $session = $collection->findOne(
            ['_id' => new MongoDB\BSON\ObjectId($this->id)],
            ['typeMap' => [
                'root' => 'array',
                'document' => 'array',
                'array' => 'array'
            ]]
        );
        $tutors = $this->allTutors();
        if (is_null($session))
            return $tutors;
        if (!isset($session['attendance']))
            return $tutors;
        $attendance = $session['attendance'];
        $tutors = array_values(array_filter($tutors, function ($elem) {
            return isset($attendance[$elem->id]);
        }));
        return $tutors;
    }

    function addExternalTutor(): bool
    {
        // TODO
        return false;
    }

    function removeExternalTutor(): bool
    {
        // TODO
        return false;
    }

    function update(): bool
    {
        // TODO
        return false;
    }

    function markAllPresent(): bool
    {
        $update = [];
        $tutors = $this->allTutors();
        foreach ($tutors as $t) {
            $update["attendance.{$t->id}"] = "p";
        }
        $collection = $this->db->selectCollection('attendance');
        $result = $collection->updateOne(
            [
                '_id' => new MongoDB\BSON\ObjectId($this->id),
                'classId' => new MongoDB\BSON\ObjectId($this->class->id)
            ],
            ['$set' => $update],
            ['upsert' => true]
        );
        return $result->isAcknowledged();
    }

    function markAllAbsent(): bool
    {
        $update = [];
        $tutors = $this->allTutors();
        foreach ($tutors as $t) {
            $update["attendance.{$t->id}"] = "a";
        }
        $collection = $this->db->selectCollection('attendance');
        $result = $collection->updateOne(
            [
                '_id' => new MongoDB\BSON\ObjectId($this->id),
                'classId' => new MongoDB\BSON\ObjectId($this->class->id)
            ],
            ['$set' => $update],
            ['upsert' => true]
        );
        return $result->isAcknowledged();
    }

    function markAllExempt(): bool
    {
        $update = [];
        $tutors = $this->allTutors();
        foreach ($tutors as $t) {
            $update["attendance.{$t->id}"] = 1;
        }
        $collection = $this->db->selectCollection('attendance');
        $result = $collection->updateOne(
            [
                '_id' => new MongoDB\BSON\ObjectId($this->id),
                'classId' => new MongoDB\BSON\ObjectId($this->class->id)
            ],
            ['$unset' => $update],
            ['upsert' => true]
        );
        return $result->isAcknowledged();
    }

    function markPresent(MTutor $tutor): bool
    {
        $collection = $this->db->selectCollection('attendance');
        $result = $collection->updateOne(
            [
                '_id' => new MongoDB\BSON\ObjectId($this->id),
                'classId' => new MongoDB\BSON\ObjectId($this->class->id)
            ],
            ['$set' => [
                "attendance.{$tutor->id}" => "p"
            ]],
            ['upsert' => true]
        );
        return $result->isAcknowledged();
    }

    function markAbsent(MTutor $tutor): bool
    {
        $collection = $this->db->selectCollection('attendance');
        $result = $collection->updateOne(
            [
                '_id' => new MongoDB\BSON\ObjectId($this->id),
                'classId' => new MongoDB\BSON\ObjectId($this->class->id)
            ],
            ['$set' => [
                "attendance.{$tutor->id}" => "a"
            ]],
            ['upsert' => true]
        );
        return $result->isAcknowledged();
    }

    function markExempt(MTutor $tutor): bool
    {
        $collection = $this->db->selectCollection('attendance');
        $result = $collection->updateOne(
            [
                '_id' => new MongoDB\BSON\ObjectId($this->id),
                'classId' => new MongoDB\BSON\ObjectId($this->class->id)
            ],
            ['$unset' => [
                "attendance.{$tutor->id}" => 1
            ]]
        );
        return $result->isAcknowledged();
    }

    function delete(): bool
    {
        // delete attendance records
        $collection = $this->db->selectCollection('attendance');
        $result = $collection->deleteOne(
            ['_id' => new MongoDB\BSON\ObjectId($this->id)]
        );
        if (!$result->isAcknowledged()) {
            return false;
        }
        // delete session data
        $collection = $this->db->selectCollection('classes');
        $result = $collection->updateOne(
            ['_id' => new \MongoDB\BSON\ObjectId($this->class->id)],
            ['$pull' => [
                'sessions' => [
                    '_id' => new \MongoDB\BSON\ObjectId($this->id)
                ]
            ]]
        );
        return $result->isAcknowledged();
    }

    function toAssoc(): array
    {
        return [
            'id' => $this->id,
            'date' => $this->date->format('Y-m-d'),
            'remarks' => $this->remarks,
            'duration' => $this->duration
        ];
    }
}
