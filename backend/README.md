# Schema

The backend is built on MongoDB, a document database. Schema is left to the server to enforce. 

Schema is as such:

```
tutors collection consists of 
{
  _id: ObjectId
  name: String
  school: String
  email: String
  dateOfBirth: Date
  dateOfRegistration: Date
  gender: "m"/"f"
  status: 0 - inactive, 1 - active, 2 - new
  admin: 0 - admin, 1 - leader, 2 - tutor
  classes: List of class ids
}

classes collection consists of 
{
  _id: ObjectId
  name: String
  year: Integer
  days: array of numbers
  timeslot: String
  duration: Float
  active: Boolean
  sessions: 
  [
    {
      _id: ObjectId
      date: Date
      duration: Float
      present: 
      [
        List of tutor ids
      ]
    }
  ]
}
```

