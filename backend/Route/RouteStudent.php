<?php

use Psr\Http\Message\ServerRequestInterface as Request;
use Slim\Http\Response as Response;
use Slim\Routing\RouteCollectorProxy;

require_once 'Model/MStudent.php';
require_once 'Model/MClass.php';
require_once 'Model/MClassTutor.php';
require_once 'Model/MClassSession.php';

$handleGetStudents = function (Request $request, Response $response, $args) {
    $queryString = $request->getUri()->getQuery();
    $queryParams = parseQueryString($queryString);
    $page = 0;
    if (isset($queryParams['page'])) {
        $page = $queryParams['page'][0];
    }
    $db = getDB();
    $students = MStudent::retrieveMany($db, $page, $queryParams);
    $students['data'] = array_map(function (MStudent $elem) {
        return $elem->toAssoc();
    }, $students['data']);
    $response = $response->withJson($students, 200);
    return $response;
};

$handleNewStudent = function (Request $request, Response $response, $args) {
    $body = $request->getParsedBody();
    $db = getDB();
    try {
        $student = MStudent::create($db, $body);
        return $response->withJson(array('id' => $student->id), 200);
    } catch (Exception $e) {
        return $response->withStatus(400, $e->getMessage());
    }
};



function getStudentRoutes($authMiddleware, $adminOnlyMiddleware)
{
    return function (RouteCollectorProxy $group) use ($authMiddleware, $adminOnlyMiddleware) {
        $respondWithSuccess = function (Request $request, Response $response, $args) {
            return $response->withStatus(200);
        };

        $group->get('', function (Request $request, Response $response, $args) {
            // get student details
            $db = getDB();
            $studentId = $args['id'];
            $student = MStudent::retrieve($db, $studentId);
            if ($student == null) {
                return $response->withStatus(400);
            }
            return $response->withJson($student->toAssoc(), 200);
        })->add($authMiddleware);
        $group->options('', $respondWithSuccess);

        $group->patch('', function (Request $request, Response $response, $args) {
            // update student details
            // extract required keys
            $body = $request->getParsedBody();
            $db = getDB();
            $studentId = $args['id'];
            $student = MStudent::retrieve($db, $studentId);
            $result = $student->update($body);
            if ($result) {
                return $response->withStatus(200);
            } else {
                return $response->withStatus(400, "Failed to update tutor");
            }
        })->add($authMiddleware)->add($adminOnlyMiddleware);
        $group->delete('', function (Request $request, Response $response, $args) {
            $db = getDB();
            $studentId = $args['id'];
            $student = MStudent::retrieve($db, $studentId);
            $result = $student->delete();
            if (!$result) {
                return $response->withStatus(400);
            }
            return $response->withStatus(200);
        })->add($authMiddleware)->add($adminOnlyMiddleware);

        // $group->get('/attended', function (Request $request, Response $response, $args) {
        //     // get tutor classes
        //     $db = getDB();
        //     $studentId = $args['id'];
        //     $student = MStudent::retrieve($db, $studentId);
        //     $classes = $student->getClasses();
        //     $sessions = array_merge(...array_map(function (MClass $elem) use ($student) {
        //         return $student->getClassSessions($elem);
        //     }, $classes));
        //     $sessions = array_filter($sessions, function (MClassSession $elem) use ($student) {
        //         return $elem->isStudentPresent($student);
        //     });
        //     $result = [];
        //     foreach ($sessions as $session) {
        //         $classId = $session->class->id;
        //         if (!isset($result[$classId]))
        //             $result[$classId] = [];
        //         array_push($result[$classId], $session->toAssoc());
        //     }
        //     error_log(print_r($result, true));
        //     return $response->withJson($result, 200);
        // })->add($authMiddleware);
        // $group->options('/attended', $respondWithSuccess);

        // $group->get('/classes', function (Request $request, Response $response, $args) {
        //     // get student classes
        //     $db = getDB();
        //     $studentId = $args['id'];
        //     $student = MStudent::retrieve($db, $studentId);
        //     $result = $student->getClasses();
        //     $result = array_map(function (MClass $class) {
        //         return $class->toAssoc();
        //     }, $result);
        //     return $response->withJson($result, 200);
        // })->add($authMiddleware);
        // $group->options('/classes', $respondWithSuccess);

        // $group->get('/hours', function (Request $request, Response $response, $args) {
        //     $db = getDB();
        //     $studentId = $args['id'];
        //     $student = MStudent::retrieve($db, $studentId);
        //     $classes = $student->getClasses();
        //     $result = [];
        //     foreach ($classes as $class) {
        //         $result[$class->id] = $student->getClassHours($class);
        //     }
        //     return $response->withJson($result, 200);
        // })->add($authMiddleware);
        // $group->options('/hours', $respondWithSuccess);

        // $group->get('/extended', function (Request $request, Response $response, $args) {
        //     $db = getDB();
        //     $studentId = $args['id'];
        //     $student = MStudent::retrieve($db, $studentId);
        //     return $response->withJson($student->toExtendedAssoc(), 200);
        // })->add($authMiddleware);
        // $group->options('/extended', $respondWithSuccess);
    };
}
