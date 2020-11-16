<?php

class MClassTutor
{
    public MTutor $tutor;
    public MClass $class;
    public DateTime $joinDate;
    public DateTime $leaveDate;

    function __construct(MTutor $tutor, MClass $class, DateTime $joinDate, DateTime $leaveDate)
    {
        $this->tutor = $tutor;
        $this->class = $class;
        $this->joinDate = $joinDate;
        $this->leaveDate = $leaveDate;
    }

    static function create()
    {
    }

    function update()
    {
    }

    function delete()
    {
    }
}
