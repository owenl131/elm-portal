<?php

class MClassTutor
{
    public MongoDB\Database $db;

    public MTutor $tutor;
    public MClass $class;
    public DateTime $joinDate;
    public ?DateTime $leaveDate;

    function __construct(MongoDB\Database $db, MTutor $tutor, MClass $class, DateTime $joinDate, ?DateTime $leaveDate)
    {
        $this->db = $db;
        $this->tutor = $tutor;
        $this->class = $class;
        $this->joinDate = $joinDate;
        $this->leaveDate = $leaveDate;
    }

    function toAssoc(): array
    {
        $result = [
            'id' => $this->tutor->id,
            'name' => $this->tutor->name,
            'admin' => $this->tutor->adminLvl,
            'status' => $this->tutor->status,
            'joinDate' => $this->joinDate->format('Y-m-d')
        ];
        if ($this->leaveDate != null) {
            $result['leaveDate'] = $this->leaveDate->format('Y-m-d');
        }
        return $result;
    }

    function updateJoinDate(DateTime $joinDate): bool
    {
        $collection = $this->db->selectCollection('classes');
        $result = $collection->updateOne(
            array(
                '_id' => new MongoDB\BSON\ObjectId($this->class->id),
                'tutors.id' => new MongoDB\BSON\ObjectId($this->tutor->id)
            ),
            array('$set' => array(
                'tutors.$.joinedOn' => new MongoDB\BSON\UTCDateTime($joinDate->getTimestamp() * 1000)
            ))
        );
        return $result->isAcknowledged();
    }

    function updateLeaveDate(DateTime $leaveDate): bool
    {
        $collection = $this->db->selectCollection('classes');
        $result = $collection->updateOne(
            array(
                '_id' => new MongoDB\BSON\ObjectId($this->class->id),
                'tutors.id' => new MongoDB\BSON\ObjectId($this->tutor->id)
            ),
            array('$set' => array(
                'tutors.$.leftOn' => new MongoDB\BSON\UTCDateTime($leaveDate->getTimestamp() * 1000)
            ))
        );
        return $result->isAcknowledged();
    }
}
