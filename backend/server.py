from flask import Flask, jsonify, request
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
        'year': 2020,
        'duration': 2,
        'timeslot': '8.00 - 10.00pm'
    }


def makeTutor():
    return {
        'id': 'id' + get_random_string(10),
        'name': 'Name ' + get_random_string(4),
        'email': get_random_string(6) + '@gmail.com',
        'school': get_random_string(4) + ' School',
        'dateOfBirth': '2000-02-01',
        'dateOfRegistration': '2020-01-15',
        'gender': 'm',
        'status': 1,
        'admin': 0
    }


tutors = [makeTutor() for _ in range(30)]
tutors.sort(key=lambda a: a['name'])
classes = [makeClass(i) for i in range(3)]
classTutors = [{
    'id': t['id'],
    'name': t['name'],
    'admin': 1,
    'joinDate': '2020-10-10'
}
    for t in tutors[:5]
]
present = [t['id'] for t in classTutors[:3]]
session = {
    'id': 0,
    'date': '2020-05-10',
    'remarks': 'Class',
    'duration': 3
}


@ app.route('/')
def hello_world():
    return jsonify(
        {
            "a": "Hello",
            "b": "Bye"
        }
    )


def getTutorById(id):
    for tutor in tutors:
        if tutor['id'] == id:
            return tutor
    return None


def inClass(id):
    for tutor in classTutors:
        if tutor['id'] == id:
            return True
    return False


@app.route('/suggestions')
def getSuggestions():
    nameFilter = request.args.get('filter')
    return jsonify(list(filter(lambda x: (nameFilter is None or nameFilter in x['name']) and not inClass(x['id']), tutors))[:10])


@app.route('/tutors')
def getTutors():
    page = int(request.args.get('page'))
    perPage = 10
    entries = len(tutors)
    pages = (entries // perPage) + (0 if entries % perPage == 0 else 1)
    if page is None:
        page = 0
    offset = int(perPage * page)
    return jsonify({
        'page': page,
        'perPage': perPage,
        'lastPage': pages,
        'total': entries,
        'data': tutors[offset:offset + perPage]
    })


@app.route('/classes')
def getClasses():
    page = 0
    perPage = 10
    pages = 1
    entries = len(classes)
    return jsonify({
        'page': page,
        'perPage': perPage,
        'lastPage': pages,
        'total': entries,
        'data': classes
    })


@app.route('/class/<cid>')
def getClass(cid):
    return jsonify(classes[int(cid)])


@app.route('/class/<cid>/addtutor/', methods=["POST"])
def addTutorToClass(cid):
    data = request.json
    joinDate = data['joinDate']
    tid = data['tutorId']
    t = getTutorById(tid)
    print(tid, joinDate, t)
    if joinDate is None or t is None:
        return 'Failed', 500
    classTutors.append({
        'id': t['id'],
        'name': t['name'],
        'admin': t['admin'],
        'joinDate': joinDate
    })
    return 'Ok', 200


@app.route('/class/<cid>/sessions')
def getSessions(cid):
    return jsonify([session])


@app.route('/class/<cid>/session/<sid>')
def getSessionDetails(cid, sid):
    return jsonify(session)


@app.route('/class/<cid>/session/<sid>/tutors')
def getSessionTutors(cid, sid):
    return jsonify(classTutors)


@app.route('/class/<cid>/session/<sid>/attendance')
def getSessionAttendance(cid, sid):
    return jsonify(present)


@ app.route('/class/<cid>/tutors')
def getClassTutors(cid):
    return jsonify(classTutors)


@ app.route('/tutor/<id>')
def getTutor(id):
    for tutor in tutors:
        if tutor['id'] == id:
            return jsonify(tutor)
    return "Record not found", 400


@ app.route('/class/<cid>/session/<sid>/present', methods=["POST"])
def markPresent(cid, sid):
    tid = request.data.decode('utf-8')
    present.append(tid)
    return "Ok", 200


@ app.route('/class/<cid>/session/<sid>/absent', methods=["POST"])
def markAbsent(cid, sid):
    tid = request.data.decode('utf-8')
    present.remove(tid)
    return "Ok", 200


if __name__ == '__main__':
    app.run(host='localhost', port=5000)
