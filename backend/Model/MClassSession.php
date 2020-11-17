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
            return MTutor::retrieve($this->db, $elem->id);
        }, $tutors);
        return $tutors;
    }

    /**
     * @return MTutor[]
     */
    function tutorsPresent(): array
    {
        $collection = $this->db->selectCollection('attendance');
        $attendance = $collection->findOne(
            ['_id' => new MongoDB\BSON\ObjectId($this->id)]
        );
        if (is_null($attendance))
            return [];
        if (!isset($session['attendance']))
            return [];
        $result = [];
        foreach ($session['attendance'] as $tutorId => $status) {
            if ($status == 'p') {
                array_push($result, $tutorId);
            }
        }
        $result = array_map(function ($id) {
            return MTutor::retrieve($this->db, $id);
        }, $result);
        return $result;
    }

    /**
     * @return MTutor[]
     */
    function tutorsAbsent(): array
    {
        $collection = $this->db->selectCollection('attendance');
        $attendance = $collection->findOne(
            ['_id' => new MongoDB\BSON\ObjectId($this->id)]
        );
        if (is_null($attendance))
            return [];
        if (!isset($session['attendance']))
            return [];
        $result = [];
        foreach ($session['attendance'] as $tutorId => $status) {
            if ($status == 'a') {
                array_push($result, $tutorId);
            }
        }
        $result = array_map(function ($id) {
            return MTutor::retrieve($this->db, $id);
        }, $result);
        return $result;
    }

    /**
     * @return MTutor[]
     */
    function tutorsExempt(): array
    {
        $collection = $this->db->selectCollection('attendance');
        $attendance = $collection->findOne(
            ['_id' => new MongoDB\BSON\ObjectId($this->id)]
        );
        $tutors = $this->allTutors();
        if (is_null($attendance))
            return $tutors;
        if (!isset($session['attendance']))
            return $tutors;
        $attendance = $session['attendance'];
        $tutors = array_filter($tutors, function ($elem) {
            return isset($attendance[$elem->id]);
        });
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
