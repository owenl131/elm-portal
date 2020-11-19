<?php

use Psr\Http\Message\ServerRequestInterface as Request;
use Slim\Http\Response as Response;
use Slim\Routing\RouteCollectorProxy;

require_once 'Model/MTutor.php';
require_once 'Model/MClass.php';
require_once 'Model/MClassTutor.php';
require_once 'Model/MClassSession.php';

$handleGetClasses = function (Request $request, Response $response, $args) {
    $queryString = $request->getUri()->getQuery();
    $queryParams = parseQueryString($queryString);
    $page = 0;
    if (isset($queryParams['page'])) {
        $page = $queryParams['page'][0];
    }
    $db = getDB();
    $classes = MClass::retrieveMany($db, $page, $queryParams);
    $classes['data'] = array_map(function (MClass $elem) {
        return $elem->toAssoc();
    }, $classes['data']);
    return $response->withJson($classes, 200);
};

$handleNewClass = function (Request $request, Response $response, $args) {
    $body = $request->getParsedBody();
    $db = getDB();
    $result = MClass::create($db, $body);
    return $response->withJson(array('id' => $result->id), 200);
};

function getClassRoutes($authMiddleware, $adminOnlyMiddleware, $leaderAboveMiddleware)
{
    return function (RouteCollectorProxy $group) use ($authMiddleware, $adminOnlyMiddleware, $leaderAboveMiddleware) {
        $respondWithSuccess = function (Request $request, Response $response, $args) {
            return $response->withStatus(200);
        };
        $group->options('', $respondWithSuccess);
        $group->get('', function (Request $request, Response $response, $args) {
            // get class details
            $classId = $args['id'];
            $db = getDB();
            $class = MClass::retrieve($db, $classId);
            return $response->withJson($class->toAssoc(), 200);
        })->add($authMiddleware);
        $group->patch('', function (Request $request, Response $response, $args) {
            // update class details
            $classId = $args['id'];
            $body = $request->getParsedBody();
            $db = getDB();
            $class = MClass::retrieve($db, $classId);
            $result = $class->update($body);
            if ($result) {
                return $response->withStatus(200);
            }
            return $response->withStatus(400, "Failed to update class");
        })->add($authMiddleware)->add($adminOnlyMiddleware);
        $group->delete('', function (Request $request, Response $response, $args) {
            $classId = $args['id'];
            $body = $request->getParsedBody();
            $db = getDB();
            $class = MClass::retrieve($db, $classId);
            $result = $class->delete($body);
            if ($result) {
                return $response->withStatus(200);
            }
            return $response->withStatus(400, "Failed to update class");
        })->add($authMiddleware)->add($adminOnlyMiddleware);

        $group->get('/tutors', function (Request $request, Response $response, $args) {
            $classId = $args['id'];
            $db = getDB();
            $class = MClass::retrieve($db, $classId);
            $tutors = $class->getTutors();
            $tutors = array_map(function (MClassTutor $elem) {
                return $elem->toAssoc();
            }, $tutors);
            return $response->withJson($tutors, 200);
        })->add($authMiddleware);
        $group->options('/tutors', $respondWithSuccess);

        $group->get('/sessions', function (Request $request, Response $response, $args) {
            $db = getDB();
            $classId = $args['id'];
            $class = MClass::retrieve($db, $classId);
            $sessions = $class->getSessions();
            $sessions = array_map(function (MClassSession $elem) {
                return $elem->toAssoc();
            }, $sessions);
            return $response->withJson($sessions, 200);
        })->add($authMiddleware);
        $group->options('/sessions', $respondWithSuccess);

        $group->post('/addsession', function (Request $request, Response $response, $args) {
            $db = getDB();
            $body = $request->getParsedBody();
            $classId = $args['id'];
            $class = MClass::retrieve($db, $classId);
            $sess = $class->addSession($body);
            return $response->withJson(array('id' => $sess->id), 200);
        })->add($authMiddleware)->add($adminOnlyMiddleware);
        $group->options('/addsession', $respondWithSuccess);

        $group->post('/addtutor', function (Request $request, Response $response, $args) {
            // add tutor to class
            $db = getDB();
            $data = $request->getParsedBody();
            $classId = $args['id'];
            $class = MClass::retrieve($db, $classId);
            $tutorId = $data['tutorId'];
            $tutor = MTutor::retrieve($db, $tutorId);
            $date = $data['joinDate'];
            $result = $class->addTutor($tutor, new DateTime($date));
            if ($result) {
                return $response->withStatus(200);
            } else {
                return $response->withStatus(400);
            }
        })->add($authMiddleware)->add($adminOnlyMiddleware);
        $group->options('/addtutor', $respondWithSuccess);

        $group->get('/suggestions', function (Request $request, Response $response, $args) {
            $db = getDB();
            $classId = $args['id'];
            $class = MClass::retrieve($db, $classId);
            $tutors = $class->getTutors();
            $tutorIds = array_map(function (MClassTutor $elem) {
                return new MongoDB\BSON\ObjectId($elem->tutor->id);
            }, $tutors);
            $filter = [];
            if (isset($request->getQueryParams()['filter'])) {
                $filter['name'] = [$request->getQueryParams()['filter']];
            }
            $filter['excludeIds'] = $tutorIds;
            $data = MTutor::retrieveMany($db, 0, $filter)['data'];
            $data = array_map(function (MTutor $elem) {
                return $elem->toAssoc();
            }, $data);
            return $response->withJson($data, 200);
        }); //->add($authMiddleware)->add($adminOnlyMiddleware);
        $group->options('/suggestions', $respondWithSuccess);

        $group->put('/updatetutor/{tid:[0-9a-z]+}', function (Request $request, Response $response, $args) {
            // used to edit join date or leave date
            $body = $request->getParsedBody();
            $db = getDB();
            $classId = $args['id'];
            $class = MClass::retrieve($db, $classId);
            $tutorId = $args['tid'];
            $tutors = $class->getTutors();
            $tutors = array_filter($tutors, function (MClassTutor $t) use ($tutorId) {
                return $t->tutor->id == $tutorId;
            });
            if (count($tutors) == 0) {
                return $response->withStatus(400);
            }
            $tutor = $tutors[0];
            if (isset($body['joinDate'])) {
                $result = $tutor->updateJoinDate(new DateTime($body['joinDate']));
                if (!$result) {
                    return $response->withStatus(400);
                }
            }
            if (isset($body['leaveDate'])) {
                $result = $tutor->updateLeaveDate(new DateTime($body['leaveDate']));
                if (!$result) {
                    return $response->withStatus(400);
                }
            }
            return $response->withStatus(200);
        })->add($authMiddleware)->add($adminOnlyMiddleware);
        $group->options('/updatetutor/{tid:[0-9a-z]+}', $respondWithSuccess);

        $group->delete('/removetutor/{tid:[0-9a-z]+}', function (Request $request, Response $response, $args) {
            // used to delete tutor from class, clears attendance records
            $db = getDB();
            $classId = $args['id'];
            $class = MClass::retrieve($db, $classId);
            $tutorId = $args['tid'];
            $tutor = MTutor::retrieve($db, $tutorId);
            if (!$class->hasTutor($tutor)) {
                return $response->withStatus(400);
            }
            $result = $class->removeTutor($tutor);
            if (!$result) {
                return $response->withStatus(400);
            }
            return $response->withStatus(200);
        })->add($authMiddleware)->add($adminOnlyMiddleware);
        $group->options('/removetutor/{tid:[0-9a-z]+}', $respondWithSuccess);

        /**
         * Functions operating on a specific session
         */
        $group->group('/session/{sid:[0-9a-z]+}', function (RouteCollectorProxy $subgroup) use ($authMiddleware, $leaderAboveMiddleware) {
            $respondWithSuccess = function (Request $request, Response $response, $args) {
                return $response->withStatus(200);
            };
            /**
             * Get session details
             */
            $subgroup->options('', $respondWithSuccess);
            $subgroup->get('', function (Request $request, Response $response, $args) {
                // get session details
                $db = getDB();
                $classId = $args['id'];
                $class = MClass::retrieve($db, $classId);
                $sessionId = $args['sid'];
                $session = $class->getSession($sessionId);
                return $response->withJson($session->toAssoc(), 200);
            })->add($authMiddleware);

            $subgroup->delete('', function (Request $request, Response $response, $args) {
                $db = getDB();
                $classId = $args['id'];
                $class = MClass::retrieve($db, $classId);
                $sessionId = $args['sid'];
                $session = $class->getSession($sessionId);
                $result = $session->delete();
                if ($result) {
                    return $response->withStatus(200);
                } else {
                    return $response->withStatus(400);
                }
            });

            $subgroup->patch('', function (Request $request, Response $response, $args) {
                // update session details
                return $response;
            })->add($authMiddleware)->add($leaderAboveMiddleware);

            $subgroup->get('/tutors', function (Request $request, Response $response, $args) {
                // get list of tutors in session
                $db = getDB();
                $classId = $args['id'];
                $class = MClass::retrieve($db, $classId);
                $sessionId = $args['sid'];
                $session = $class->getSession($sessionId);
                $tutors = $session->allClassTutors();
                $tutors = array_values(array_map(function (MClassTutor $elem) {
                    return $elem->toAssoc();
                }, $tutors));
                return $response->withJson($tutors, 200);
            })->add($authMiddleware);
            $subgroup->options('/tutors', $respondWithSuccess);

            $subgroup->get('/present', function (Request $request, Response $response, $args) {
                // get list of tutors present in session
                $db = getDB();
                $classId = $args['id'];
                $class = MClass::retrieve($db, $classId);
                $sessionId = $args['sid'];
                $session = $class->getSession($sessionId);
                $tutors = $session->tutorsPresent();
                $tutors = array_map(function (MTutor $elem) {
                    return $elem->id;
                }, $tutors);
                return $response->withJson($tutors, 200);
            })->add($authMiddleware);
            $subgroup->options('/present', $respondWithSuccess);

            $subgroup->get('/absent', function (Request $request, Response $response, $args) {
                // get list of tutors absent in session
                $db = getDB();
                $classId = $args['id'];
                $class = MClass::retrieve($db, $classId);
                $sessionId = $args['sid'];
                $session = $class->getSession($sessionId);
                $tutors = $session->tutorsAbsent();
                $tutors = array_map(function (MTutor $elem) {
                    return $elem->id;
                }, $tutors);
                return $response->withJson($tutors, 200);
            })->add($authMiddleware);
            $subgroup->options('/absent', $respondWithSuccess);

            $subgroup->get('/exempt', function (Request $request, Response $response, $args) {
                // get list of tutors exempt in session
                $db = getDB();
                $classId = $args['id'];
                $class = MClass::retrieve($db, $classId);
                $sessionId = $args['sid'];
                $session = $class->getSession($sessionId);
                $tutors = $session->tutorsExempt();
                $tutors = array_map(function (MTutor $elem) {
                    return $elem->id;
                }, $tutors);
                return $response->withJson($tutors, 200);
            })->add($authMiddleware);
            $subgroup->options('/exempt', $respondWithSuccess);

            $subgroup->post('/present/{tid:[0-9a-z]+}', function (Request $request, Response $response, $args) {
                // mark tutor as present
                $db = getDB();
                $classId = $args['id'];
                $class = MClass::retrieve($db, $classId);
                $sessionId = $args['sid'];
                $session = $class->getSession($sessionId);
                $tutorId = $args['tid'];
                $tutor = MTutor::retrieve($db, $tutorId);
                if ($tutor == null) {
                    return $response->withStatus(400, "Tutor not found");
                }
                if (!$session->hasTutor($tutor)) {
                    return $response->withStatus(400, "Tutor not in session");
                }
                $result = $session->markPresent($tutor);
                if (!$result) {
                    return $response->withStatus(400);
                }
                return $response->withStatus(200);
            })->add($authMiddleware)->add($leaderAboveMiddleware);
            $subgroup->options('/present/{tid:[0-9a-z]+}', $respondWithSuccess);

            $subgroup->post('/absent/{tid:[0-9a-z]+}', function (Request $request, Response $response, $args) {
                // mark tutor as absent
                $db = getDB();
                $classId = $args['id'];
                $class = MClass::retrieve($db, $classId);
                $sessionId = $args['sid'];
                $session = $class->getSession($sessionId);
                $tutorId = $args['tid'];
                $tutor = MTutor::retrieve($db, $tutorId);
                if ($tutor == null) {
                    return $response->withStatus(400, "Tutor not found");
                }
                if (!$session->hasTutor($tutor)) {
                    return $response->withStatus(400, "Tutor not in session");
                }
                $result = $session->markAbsent($tutor);
                if (!$result) {
                    return $response->withStatus(400);
                }
                return $response->withStatus(200);
            })->add($authMiddleware)->add($leaderAboveMiddleware);
            $subgroup->options('/absent/{tid:[0-9a-z]+}', $respondWithSuccess);

            $subgroup->post('/exempt/{tid:[0-9a-z]+}', function (Request $request, Response $response, $args) {
                // mark tutor as absent
                $db = getDB();
                $classId = $args['id'];
                $class = MClass::retrieve($db, $classId);
                $sessionId = $args['sid'];
                $session = $class->getSession($sessionId);
                $tutorId = $args['tid'];
                $tutor = MTutor::retrieve($db, $tutorId);
                if ($tutor == null) {
                    return $response->withStatus(400, "Tutor not found");
                }
                if (!$session->hasTutor($tutor)) {
                    return $response->withStatus(400, "Tutor not in session");
                }
                $result = $session->markExempt($tutor);
                if (!$result) {
                    return $response->withStatus(400);
                }
                return $response->withStatus(200);
            })->add($authMiddleware)->add($leaderAboveMiddleware);
            $subgroup->options('/exempt/{tid:[0-9a-z]+}', $respondWithSuccess);

            $subgroup->put('/addexternal/{tid:[0-9a-z]+}', function (Request $request, Response $response, $args) {
                // add a tutor that is not under this class to this session
                // TODO
                return $response;
            })->add($authMiddleware)->add($leaderAboveMiddleware);

            $subgroup->put('/removeexternal/{tid:[0-9a-z]+}', function (Request $request, Response $response, $args) {
                // remove a tutor that is not under this class from this session
                // TODO
                return $response;
            })->add($authMiddleware)->add($leaderAboveMiddleware);
        });
    };
}
