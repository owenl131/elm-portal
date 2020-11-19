<?php

use Psr\Http\Message\ServerRequestInterface as Request;
use Slim\Http\Response as Response;
use Slim\Routing\RouteCollectorProxy;

require_once 'Model/MTutor.php';
require_once 'Model/MClass.php';
require_once 'Model/MClassTutor.php';
require_once 'Model/MClassSession.php';

function getUserRoutes($authMiddleware)
{
    return function (RouteCollectorProxy $group) use ($authMiddleware) {
        // for actions for currently logged in user

        $group->get('/details', function (Request $request, Response $response, $args) {
            // get own profile
            $tutorId = $request->getAttribute('tutorId');
            $db = getDB();
            $tutor = MTutor::retrieve($db, $tutorId);
            if ($tutor == null) {
                return $response->withStatus(400);
            }
            return $response->withJson($tutor->toAssoc(), 200);
        })->add($authMiddleware);

        $group->options('/details', function (Request $request, Response $response, $args) {
            return $response->withStatus(200);
        });

        $group->patch('/details', function (Request $request, Response $response, $args) {
            // update own profile
            return $response;
        })->add($authMiddleware);

        $group->get('/classes', function (Request $request, Response $response, $args) {
            // get own classes (for home page)
            $db = getDB();
            $tutorId = $request->getAttribute("tutorId");
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
    };
}
