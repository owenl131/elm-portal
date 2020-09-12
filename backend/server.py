from flask import Flask, jsonify
from flask_cors import CORS


app = Flask(__name__)
CORS(app)

sampleSession1 = {
    'id': 'session123',
    'date': '2020-09-10',
    'remarks': 'Class'
}
sampleSession2 = {
    'id': 'session124',
    'date': '2020-09-03',
    'remarks': 'Class'
}
sampleClass = {
    'id': 'class123',
    'name': 'P4 Math',
    'timeslot': 'Monday',
    'year': 2020,
    'sessions': [
        sampleSession1,
        sampleSession2
    ]
}
sampleTutor = {
    'id': 'tutor123',
    'name': 'ABC Tan',
    'classes': [
        sampleClass
    ]
}

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
    return jsonify([sampleTutor])

if __name__ == '__main__':
    app.run(host='localhost', port=5000)