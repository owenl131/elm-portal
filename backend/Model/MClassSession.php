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
        // TODO
    }

    function tutorsPresent()
    {
        // TODO
    }

    function tutorsAbsent()
    {
        // TODO
    }

    function tutorsExempt()
    {
        // TODO
    }

    function addExternalTutor()
    {
        // TODO
    }

    function removeExternalTutor()
    {
        // TODO
    }

    function update()
    {
        // TODO
    }

    function markPresent()
    {
        // TODO
    }

    function markAbsent()
    {
        // TODO
    }

    function markExempt()
    {
        // TODO
    }

    function delete()
    {
        // TODO
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
