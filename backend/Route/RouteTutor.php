<?php

use Psr\Http\Message\ServerRequestInterface as Request;
use Slim\Http\Response as Response;
use Slim\Routing\RouteCollectorProxy;

require_once 'Model/MTutor.php';
require_once 'Model/MClass.php';
require_once 'Model/MClassTutor.php';
require_once 'Model/MClassSession.php';

$handleGetTutors = function (Request $request, Response $response, $args) {
    $queryString = $request->getUri()->getQuery();
    $queryParams = parseQueryString($queryString);
    $page = 0;
    if (isset($queryParams['page'])) {
        $page = $queryParams['page'][0];
    }
    $db = getDB();
    $tutors = MTutor::retrieveMany($db, $page, $queryParams);
    $tutors['data'] = array_map(function (MTutor $elem) {
        return $elem->toAssoc();
    }, $tutors['data']);
    $response = $response->withJson($tutors, 200);
    return $response;
};

$handleNewTutor = function (Request $request, Response $response, $args) {
    $body = $request->getParsedBody();
    $db = getDB();
    try {
        $tutor = MTutor::create($db, $body);
        return $response->withJson(array('id' => $tutor->id), 200);
    } catch (Exception $e) {
        $response->withStatus(400, $e->getMessage());
    }
};

function getTutorRoutes($authMiddleware, $adminOnlyMiddleware)
{
    return function (RouteCollectorProxy $group) use ($authMiddleware, $adminOnlyMiddleware) {

        $group->get('', function (Request $request, Response $response, $args) {
            // get tutor details
            $tutorId = $args['id'];
            $db = getDB();
            $tutor = MTutor::retrieve($db, $tutorId);
            if ($tutor == null) {
                return $response->withStatus(400);
            }
            return $response->withJson($tutor->toAssoc(), 200);
        })->add($authMiddleware);
        $group->options('', function (Request $request, Response $response, $args) {
            return $response->withStatus(200);
        });

        $group->patch('', function (Request $request, Response $response, $args) {
            // update tutor details$body = $request->getParsedBody();
            // extract required keys
            $body = $request->getParsedBody();
            $body['dob'] = $body['dateOfBirth'];
            $body['doc'] = $body['dateOfRegistration'];
            $db = getDB();
            $tutorId = $args['id'];
            $tutor = MTutor::retrieve($db, $tutorId);
            $result = $tutor->update($body);
            if ($result) {
                return $response->withStatus(200);
            } else {
                return $response->withStatus(400, "Failed to update tutor");
            }
        })->add($authMiddleware)->add($adminOnlyMiddleware);

        $group->get('/attended', function (Request $request, Response $response, $args) {
            // get tutor classes
            $db = getDB();
            $tutorId = $args['id'];
            $tutor = MTutor::retrieve($db, $tutorId);
            $classes = $tutor->getClasses();
            $sessions = array_merge(array_map(function (MClass $elem) use ($tutor) {
                return $tutor->getClassSessions($elem);
            }, $classes));
            $sessions = array_filter($sessions, function (MClassSession $elem) use ($tutor) {
                return $elem->isTutorPresent($tutor);
            });
            $sessions = array_map(function (MClassSession $elem) {
                return $elem->toAssoc();
            }, $sessions);
            return $response->withJson($sessions, 200);
        })->add($authMiddleware);
        $group->options('/attended', function (Request $request, Response $response, $args) {
            return $response->withStatus(200);
        });

        $group->get('/classes', function (Request $request, Response $response, $args) {
            // get tutor classes
            $db = getDB();
            $tutorId = $args['id'];
            $tutor = MTutor::retrieve($db, $tutorId);
            $result = $tutor->getClasses();
            $result = array_map(function (MClass $class) {
                return $class->toAssoc();
            }, $result);
            return $response->withJson($result, 200);
        })->add($authMiddleware);
        $group->options('/classes', function (Request $request, Response $response, $args) {
            return $response->withStatus(200);
        });

        $group->get('/hours', function (Request $request, Response $response, $args) {
            $db = getDB();
            $tutorId = $args['id'];
            $tutor = MTutor::retrieve($db, $tutorId);
            $classes = $tutor->getClasses();
            $result = [];
            foreach ($classes as $class) {
                $result[$class->id] = $tutor->getClassHours($class);
            }
            return $response->withJson($result, 200);
        })->add($authMiddleware);
        $group->options('/hours', function (Request $request, Response $response, $args) {
            return $response->withStatus(200);
        });
    };
}
