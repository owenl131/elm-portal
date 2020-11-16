<?php

class MClassSession
{
    public MClass $class;
    public string $id;
    public DateTime $date;
    public string $remarks;
    public float $duration;

    function __construct(MClass $class, string $id, DateTime $date, string $remarks, float $duration)
    {
        $this->class = $class;
        $this->id = $id;
        $this->date = $date;
        $this->remarks = $remarks;
        $this->duration = $duration;
    }

    function allTutors()
    {
    }

    function tutorsPresent()
    {
    }

    function tutorsAbsent()
    {
    }

    function tutorsExempt()
    {
    }

    function addExternalTutor()
    {
    }

    function removeExternalTutor()
    {
    }

    function update()
    {
    }

    function delete()
    {
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
