from flask import Flask, jsonify
from flask_cors import CORS
import random
import string

def get_random_string(length):
    letters = string.ascii_lowercase
    result_str = ''.join(random.choice(letters) for i in range(length))
    return result_str

app = Flask(__name__)
CORS(app)

def makeClass(id):
    return {
        'id': id,
        'name': 'Class ' + get_random_string(4),
        'days': [1, 2],
        'active': True,
        'duration': 2,
        'timeslot': '8.00 - 10.00pm'
    }

def makeTutor():
    return {
        'id': 'id' + get_random_string(10),
        'name': 'Name ' + get_random_string(4),
        'email': get_random_string(6) + '@gmail.com',
        'school': get_random_string(4) +  ' School',
        'dateOfBirth': '2000-02-01',
        'dateOfRegistration': '2020-01-15',
        'gender': 'm',
        'status': 1,
        'admin': 0
    }

tutors = [makeTutor() for _ in range(30)]
classes = [makeClass(i) for i in range(3)]

@app.route('/')
def hello_world():
    return jsonify(
        {
            "a": "Hello",
            "b": "Bye"
        }
    )

@app.route('/tutors')
def getTutors():
    return jsonify(tutors[:10])

@app.route('/classes')
def getClasses():
    return jsonify(classes)

@app.route('/tutor/<id>')
def getTutor(id):
    for tutor in tutors:
        if tutor['id'] == id:
            return jsonify(tutor)
    return "Record not found", 400

if __name__ == '__main__':
    app.run(host='localhost', port=5000)