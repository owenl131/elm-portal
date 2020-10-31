<?php

use Psr\Http\Message\ServerRequestInterface as Request;
use Psr\Http\Server\RequestHandlerInterface as RequestHandler;
use Slim\Factory\AppFactory;
use Slim\Http\Response as Response;
use Slim\Psr7\Factory\ResponseFactory;
use Slim\Routing\RouteCollectorProxy;
use Slim\Routing\RouteContext;

require 'Model/DBTutor.php';
require 'Model/DBClass.php';
require __DIR__ . '/../vendor/autoload.php';


$app = AppFactory::create();
$app->add(function (Request $request, RequestHandler $handler) {
    $routeContext = RouteContext::fromRequest($request);
    $routingResults = $routeContext->getRoutingResults();
    $methods = $routingResults->getAllowedMethods();
    $requestHeaders = $request->getHeaderLine('Access-Control-Request-Headers');

    $response = $handler->handle($request);

    $response = $response
        ->withHeader('Access-Control-Allow-Origin', 'http://localhost:8000')
        ->withHeader('Access-Control-Allow-Methods', implode(', ', $methods))
        ->withHeader('Access-Control-Allow-Headers', $requestHeaders ?: '*');

    $response = $response->withHeader('Access-Control-Allow-Credentials', 'true');

    return $response;
});
$app->addRoutingMiddleware();


function parseQueryString($queryString)
{
    $query = explode('&', $queryString);
    $params = array();
    foreach ($query as $param) {
        if (strpos($param, '=') === false)
            $param .= '=';
        list($name, $value) = explode('=', $param, 2);
        $params[urldecode($name)][] = urldecode($value);
    }
    return $params;
}


$authMiddleware = function (Request $request, RequestHandler $handler) {
    // authenticates user
    $authLine = $request->getHeaderLine("Authorization");
    if (preg_match("/Bearer\s+(.*)$/i", $request->getHeaderLine("Authorization"), $matches)) {
        $sessionId = base64_decode($matches[1]);
        $result = DBTutor::authenticateBySessionId($sessionId);
        if ($result) {
            $request = $request
                ->withAttribute('sessionId', $sessionId)
                ->withAttribute('tutorId', $result);
            $response = $handler->handle($request);
            return $response;
        }
    }
    return (new ResponseFactory)->createResponse(401);
};


$leaderAboveMiddleware = function (Request $request, RequestHandler $handler) {
    // TODO implement
    $response = $handler->handle($request);
    return $response;
};


$adminOnlyMiddleware = function (Request $request, RequestHandler $handler) {
    // insert for functions requiring admin level rights
    // TODO implement
    $response = $handler->handle($request);
    return $response;
};


$app->get('/', function (Request $request, Response $response, $args) {
    $response->getBody()->write('Hello world!');
    return $response;
});


$app->post('/auth', function (Request $request, Response $response, $args) {
    $parsedBody = $request->getParsedBody();
    $email = $parsedBody['email'];
    $password = $parsedBody['password'];
    $result = DBTutor::authenticateByCredentials($email, $password);
    if (!$result) {
        return $response->withStatus(401);
    }
    return $response->withJson(
        array(
            'email' => $email,
            'session' => $result['session'],
            'sessionExpiry' => $result['sessionExpiry']
        ),
        200
    );
});
$app->options('/auth', function (Request $request, Response $response, $args) {
    return $response->withStatus(200);
});


$app->get('/tutors', function (Request $request, Response $response, $args) {
    // gets list of tutors
    $queryString = $request->getUri()->getQuery();
    $queryParams = parseQueryString($queryString);
    $page = 0;
    if (isset($queryParams['page'])) {
        $page = $queryParams['page'][0];
    }
    $data = DBTutor::getTutorList($page, $queryParams);
    $data['data'] = array_map(function ($elem) {
        $elem['id'] = (string) $elem['_id'];
        unset($elem['_id']);
        $elem['dateOfBirth'] = $elem['dob']->toDateTime()->format('Y-m-d');
        unset($elem['dob']);
        $elem['dateOfRegistration'] = $elem['doc']->toDateTime()->format('Y-m-d');
        unset($elem['doc']);
        if (isset($elem['sessionId']))
            unset($elem['sessionId']);
        if (isset($elem['sessionExpiry']))
            unset($elem['sessionExpiry']);
        return $elem;
    }, $data['data']);
    $response = $response->withJson($data, 200);
    return $response;
})->add($authMiddleware);

$app->options('/tutors', function (Request $request, Response $response, $args) {
    return $response->withStatus(200);
});

$app->post('/tutors/new', function (Request $request, Response $response, $args) {
    $body = $request->getParsedBody();
    // extract required keys
    $tutor = array();
    if (!isset($body['name']) || strlen($body['name']) == 0) {
        return $response->withStatus(400, "Name must be given");
    }
    $tutor['name'] = $body['name'];
    if (!isset($body['school']) || strlen($body['school']) == 0) {
        return $response->withStatus(400, "School must be given");
    }
    $tutor['school'] = $body['school'];
    if (!isset($body['admin']) || !is_int($body['admin']) || $body['admin'] < 0 || $body['admin'] > 1) {
        return $response->withStatus(400, "Invalid admin level");
    }
    $tutor['admin'] = $body['admin'];
    if (!isset($body['gender']) || ($body['gender'] !== 'm' && $body['gender'] !== 'f')) {
        return $response->withStatus(400, "Invalid gender");
    }
    $tutor['gender'] = $body['gender'];
    if (!isset($body['status']) || !is_int($body['status']) || $body['status'] < 0 || $body['status'] > 2) {
        return $response->withStatus(400, "Invalid status");
    }
    $tutor['status'] = $body['status'];
    if (!isset($body['email']) || strlen($body['email']) == 0) {
        return $response->withStatus(400, "Email must be given");
    }
    $tutor['email'] = $body['email'];
    if (!isset($body['password']) || strlen($body['password']) < 8) {
        return $response->withStatus(400, "Password must be given");
    }
    $tutor['password'] = $body['password'];
    if (!strtotime($body['dateOfBirth'])) {
        return $response->withStatus(400, "Invalid date-of-birth");
    }
    if (!strtotime($body['dateOfRegistration'])) {
        return $response->withStatus(400, "Invalid date-of-registration");
    }
    $tutor['dob'] = new \MongoDB\BSON\UTCDateTime(strtotime($body['dateOfBirth']) * 1000);
    $tutor['doc'] = new \MongoDB\BSON\UTCDateTime(strtotime($body['dateOfRegistration']) * 1000);
    $newId = DBTutor::addTutor($tutor);
    if (!$newId) {
        return $response->withStatus(400, "Failed to add tutor");
    }
    return $response->withJson(array('id' => $newId), 200);
});
$app->options('/tutors/new', function (Request $request, Response $response, $args) {
    return $response->withStatus(200);
});

$app->get('/tutorstats', function (Request $request, Response $response, $args) {
    // returns summary stats by age, school, commencement date, school type, languages spoken, available day
    return $response;
})->add($authMiddleware);


$app->group('/my', function (RouteCollectorProxy $group) use ($authMiddleware) {
    // for actions for currently logged in user

    $group->get('/details', function (Request $request, Response $response, $args) {
        // get own profile
        $tutorId = $request->getAttribute('tutorId');
        $tutorData = DBTutor::getTutor($tutorId);
        if ($tutorData == null) {
            return $response->withStatus(400);
        }
        $tutorData['id'] = (string) $tutorData['_id'];
        unset($tutorData['_id']);
        $tutorData['dateOfBirth'] = $tutorData['dob']->toDateTime()->format('Y-m-d');
        unset($tutorData['dob']);
        $tutorData['dateOfRegistration'] = $tutorData['doc']->toDateTime()->format('Y-m-d');
        unset($tutorData['doc']);
        unset($tutorData['sessionId']);
        unset($tutorData['sessionExpiry']);
        return $response->withJson($tutorData, 200);
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
        $tutorId = $request->getAttribute("tutorId");
        $result = DBTutor::getClasses($tutorId);
        $result = array_map(function ($class) {
            $class['id'] = (string) $class['_id'];
            unset($class['_id']);
            return $class;
        }, $result);
        return $response->withJson($result, 200);
    })->add($authMiddleware);
    $group->options('/classes', function (Request $request, Response $response, $args) {
        return $response->withStatus(200);
    });
});


$app->group('/tutor/{id:[0-9a-z]+}', function (RouteCollectorProxy $group) use ($authMiddleware, $adminOnlyMiddleware) {

    $group->get('', function (Request $request, Response $response, $args) {
        // get tutor details
        $tutorId = $args['id'];
        $tutorData = DBTutor::getTutor($tutorId);
        if ($tutorData == null) {
            return $response->withStatus(400);
        }
        $tutorData['id'] = (string) $tutorData['_id'];
        unset($tutorData['_id']);
        $tutorData['dateOfBirth'] = $tutorData['dob']->toDateTime()->format('Y-m-d');
        unset($tutorData['dob']);
        $tutorData['dateOfRegistration'] = $tutorData['doc']->toDateTime()->format('Y-m-d');
        unset($tutorData['doc']);
        if (isset($tutorData['sessionId']))
            unset($tutorData['sessionId']);
        if (isset($tutorData['sessionExpiry']))
            unset($tutorData['sessionExpiry']);
        return $response->withJson($tutorData, 200);
    })->add($authMiddleware);
    $group->options('', function (Request $request, Response $response, $args) {
        return $response->withStatus(200);
    });

    $group->patch('', function (Request $request, Response $response, $args) {
        // update tutor details$body = $request->getParsedBody();
        // extract required keys
        $body = $request->getParsedBody();
        $tutor = array();
        if (!isset($body['name']) || strlen($body['name']) == 0) {
            return $response->withStatus(400, "Name must be given");
        }
        $tutor['name'] = $body['name'];
        if (!isset($body['school']) || strlen($body['school']) == 0) {
            return $response->withStatus(400, "School must be given");
        }
        $tutor['school'] = $body['school'];
        if (!isset($body['admin']) || !is_int($body['admin']) || $body['admin'] < 0 || $body['admin'] > 1) {
            return $response->withStatus(400, "Invalid admin level");
        }
        $tutor['admin'] = $body['admin'];
        if (!isset($body['gender']) || ($body['gender'] !== 'm' && $body['gender'] !== 'f')) {
            return $response->withStatus(400, "Invalid gender");
        }
        $tutor['gender'] = $body['gender'];
        if (!isset($body['status']) || !is_int($body['status']) || $body['status'] < 0 || $body['status'] > 2) {
            return $response->withStatus(400, "Invalid status");
        }
        $tutor['status'] = $body['status'];
        if (!isset($body['email']) || strlen($body['email']) == 0) {
            return $response->withStatus(400, "Email must be given");
        }
        $tutor['email'] = $body['email'];
        if (isset($body['password'])) {
            $tutor['password'] = $body['password'];
            if (strlen($body['password']) < 8)
                return $response->withStatus(400, "Password must be given");
        }
        if (!strtotime($body['dateOfBirth'])) {
            return $response->withStatus(400, "Invalid date-of-birth");
        }
        if (!strtotime($body['dateOfRegistration'])) {
            return $response->withStatus(400, "Invalid date-of-registration");
        }
        $tutor['dob'] = new \MongoDB\BSON\UTCDateTime(strtotime($body['dateOfBirth']) * 1000);
        $tutor['doc'] = new \MongoDB\BSON\UTCDateTime(strtotime($body['dateOfRegistration']) * 1000);
        $tutorId = $args['id'];
        DBTutor::updateTutorDetails($tutorId, $tutor);
        return $response->withStatus(200);
    })->add($authMiddleware)->add($adminOnlyMiddleware);

    $group->get('/classes', function (Request $request, Response $response, $args) {
        // get tutor classes
        $tutorId = $args['id'];
        $result = DBTutor::getClasses($tutorId);
        $result = array_map(function ($class) {
            $class['id'] = (string) $class['_id'];
            unset($class['_id']);
            return $class;
        }, $result);
        return $response->withJson($result, 200);
    })->add($authMiddleware);
    $group->options('/classes', function (Request $request, Response $response, $args) {
        return $response->withStatus(200);
    });
});

/**
 * Get class list
 */
$app->get('/classes', function (Request $request, Response $response, $args) {
    $queryString = $request->getUri()->getQuery();
    $queryParams = parseQueryString($queryString);
    $page = 0;
    if (isset($queryParams['page'])) {
        $page = $queryParams['page'][0];
    }
    $data = DBClass::getClassList($page, $queryParams);
    $data['data'] = array_map(function ($elem) {
        $elem['id'] = (string) $elem['_id'];
        unset($elem['_id']);
        return $elem;
    }, $data['data']);
    return $response->withJson($data, 200);
})->add($authMiddleware);

$app->options('/classes', function (Request $request, Response $response, $args) {
    return $response->withStatus(200);
});

$app->post('/classes/new', function (Request $request, Response $response, $args) {
    $body = $request->getParsedBody();
    $class = array();
    if (!isset($body['name']) || strlen($body['name']) == 0) {
        return $response->withStatus(400, "Name must be given");
    }
    $class['name'] = (string) $body['name'];
    if (!isset($body['duration'])) {
        $class['duration'] = 3;
    } else {
        $class['duration'] = floatval($body['duration']);
    }
    if (!isset($body['year'])) {
        $class['year'] = intval(date("Y"));
    } else {
        $class['year'] = intval($body['year']);
        if ($class['year'] > 2100 || $class['year'] < 2000) {
            return $response->withStatus(400, "Invalid year");
        }
    }
    $class['timeslot'] = $body['timeslot'] ?? "";
    $class['active'] = boolval($body['active'] ?? true);
    $class['days'] = $body['days'] ?? array();
    $newId = DBClass::addClass($class);
    if (!$newId) {
        return $response->withStatus(400, "Failed to add tutor");
    }
    return $response->withJson(array('id' => $newId), 200);
});
$app->options('/classes/new', function (Request $request, Response $response, $args) {
    return $response->withStatus(200);
});

$app->get('/classestoday', function (Request $Request, Response $response, $args) {
    // get all active classes with classes on this weekday with the most recent class at most 2 weeks ago
    return $response;
})->add($authMiddleware);


$app->group('/class/{id:[a-z0-9]+}', function (RouteCollectorProxy $group) use ($authMiddleware, $adminOnlyMiddleware, $leaderAboveMiddleware) {

    $group->get('', function (Request $request, Response $response, $args) {
        // get class details
        $classId = $args['id'];
        $data = DBClass::getClass($classId);
        if ($data == null) {
            return $response->withStatus(400);
        }
        $data['id'] = (string) $data['_id'];
        unset($data['_id']);
        return $response->withJson($data, 200);
    })->add($authMiddleware);
    $group->options('', function (Request $request, Response $response, $args) {
        return $response->withStatus(200);
    });

    $group->patch('', function (Request $request, Response $response, $args) {
        // update class details
        $classId = $args['id'];
        $body = $request->getParsedBody();
        $result = DBClass::updateClassDetails($classId, $body);
        if ($result) {
            return $response->withStatus(200);
        }
        return $response->withStatus(400, "Failed to update class");
    })->add($authMiddleware)->add($adminOnlyMiddleware);

    $group->get('/tutors', function (Request $request, Response $response, $args) {
        $classId = $args['id'];
        $data = DBClass::getTutors($classId);
        return $response->withJson($data, 200);
    })->add($authMiddleware);
    $group->options('/tutors', function (Request $request, Response $response, $args) {
        return $response->withStatus(200);
    });

    $group->get('/sessions', function (Request $request, Response $response, $args) {
        $classId = $args['id'];
        $data = DBClass::getSessions($classId);
        foreach ($data as &$session) {
            $session['id'] = (string) $session['_id'];
            unset($session['_id']);
            $session['date'] = $session['date']->toDateTime()->format('Y-m-d');
        }
        return $response->withJson($data, 200);
    })->add($authMiddleware);
    $group->options('/sessions', function (Request $request, Response $response, $args) {
        return $response->withStatus(200);
    });

    $group->post('/addsession', function (Request $request, Response $response, $args) {
        $classId = $args['id'];
        $body = $request->getParsedBody();
        $data = array();
        $data['date'] = new \MongoDB\BSON\UTCDateTime(strtotime($body['date']) * 1000);
        $data['remarks'] = $body['remarks'];
        $data['duration'] = $body['duration'];
        $sessionId = DBClass::addSession($classId, $data);
        return $response->withJson(array('id' => $sessionId), 200);
    })->add($authMiddleware)->add($adminOnlyMiddleware);
    $group->options('/addsession', function (Request $request, Response $response, $args) {
        return $response->withStatus(200);
    });

    $group->post('/addtutor', function (Request $request, Response $response, $args) {
        // add tutor to class
        $classId = $args['id'];
        $data = $request->getParsedBody();
        $tutorId = $data['tutorId'];
        $date = $data['joinDate'];
        $result = DBClass::addTutor($classId, $tutorId, $date);
        if ($result) {
            return $response->withStatus(200);
        } else {
            return $response->withStatus(400);
        }
    })->add($authMiddleware)->add($adminOnlyMiddleware);
    $group->options('/addtutor', function (Request $request, Response $response, $args) {
        return $response->withStatus(200);
    });

    $group->get('/suggestions', function (Request $request, Response $response, $args) {
        $classId = $args['id'];
        $filter = $request->getQueryParams()['filter'] ?? "";
        $data = DBClass::tutorSuggestions($classId, $filter);
        foreach ($data as $elem) {
            $elem['id'] = (string) $elem['_id'];
            unset($elem['_id']);
        }
        return $response->withJson($data, 200);
    }); //->add($authMiddleware)->add($adminOnlyMiddleware);
    $group->options('/suggestions', function (Request $request, Response $response, $args) {
        return $response->withStatus(200);
    });

    $group->put('/updatetutor/{tid:[0-9a-z]+}', function (Request $request, Response $response, $args) {
        // used to edit join date or leave date
        return $response;
    })->add($authMiddleware)->add($adminOnlyMiddleware);

    $group->put('/removetutor/{tid:[0-9a-z]+}', function (Request $request, Response $response, $args) {
        // used to delete tutor from class, clears attendance records
        return $response;
    })->add($authMiddleware)->add($adminOnlyMiddleware);


    $group->group('/session/{sid:[0-9a-z]+}', function (RouteCollectorProxy $subgroup) use ($authMiddleware, $leaderAboveMiddleware) {

        $subgroup->get('', function (Request $request, Response $response, $args) {
            // get session details
            $classId = $args['id'];
            $sessionId = $args['sid'];
            $session = DBClass::getSession($classId, $sessionId);
            $session['id'] = (string) $session['_id'];
            unset($session['_id']);
            $session['date'] = $session['date']->toDateTime()->format('Y-m-d');
            return $response->withJson($session, 200);
        })->add($authMiddleware);
        $subgroup->options('', function (Request $request, Response $response, $args) {
            return $response->withStatus(200);
        });
        $subgroup->delete('', function (Request $request, Response $response, $args) {
            $classId = $args['id'];
            $sessionId = $args['sid'];
            DBClass::deleteSession($classId, $sessionId);
            return $response->withStatus(200);
        });

        $subgroup->patch('', function (Request $request, Response $response, $args) {
            // update session details
            return $response;
        })->add($authMiddleware)->add($leaderAboveMiddleware);

        $subgroup->get('/tutors', function (Request $request, Response $response, $args) {
            // get list of tutors in session
            $classId = $args['id'];
            $sessionId = $args['sid'];
            $tutors = DBClass::sessionTutors($classId, $sessionId);
            $tutors = array_map(function ($elem) {
                $elem['id'] = (string) $elem['id'];
                $elem['joinDate'] = $elem['joinedOn']->toDateTime()->format('Y-m-d');
                unset($elem['joinedOn']);
                if (isset($elem['leftOn'])) {
                    $elem['leaveDate'] = $elem['leftOn']->toDateTime()->format('Y-m-d');
                    unset($elem['leftOn']);
                }
                return $elem;
            }, $tutors);
            return $response->withJson($tutors, 200);
        })->add($authMiddleware);
        $subgroup->options('/tutors', function (Request $request, Response $response, $args) {
            return $response->withStatus(200);
        });

        $subgroup->get('/present', function (Request $request, Response $response, $args) {
            // get list of tutors present in session
            $classId = $args['id'];
            $sessionId = $args['sid'];
            $tutors = DBClass::tutorsPresent($classId, $sessionId);
            $tutors = array_map(function ($elem) {
                return (string) $elem;
            }, $tutors);
            return $response->withJson($tutors, 200);
        })->add($authMiddleware);
        $subgroup->options('/present', function (Request $request, Response $response, $args) {
            return $response->withStatus(200);
        });

        $subgroup->get('/absent', function (Request $request, Response $response, $args) {
            // get list of tutors absent in session
            $classId = $args['id'];
            $sessionId = $args['sid'];
            $tutors = DBClass::tutorsAbsent($classId, $sessionId);
            $tutors = array_map(function ($elem) {
                return (string) $elem;
            }, $tutors);
            return $response->withJson($tutors, 200);
        })->add($authMiddleware);
        $subgroup->options('/absent', function (Request $request, Response $response, $args) {
            return $response->withStatus(200);
        });

        $subgroup->post('/present/{tid:[0-9a-z]+}', function (Request $request, Response $response, $args) {
            // mark tutor as present
            $classId = $args['id'];
            $sessionId = $args['sid'];
            $tutorId = $args['tid'];
            $result = DBClass::markPresent($classId, $sessionId, $tutorId);
            if (!$result) {
                return $response->withStatus(400);
            }
            return $response->withStatus(200);
        })->add($authMiddleware)->add($leaderAboveMiddleware);
        $subgroup->options('/present/{tid:[0-9a-z]+}', function (Request $request, Response $response, $args) {
            return $response->withStatus(200);
        });

        $subgroup->post('/absent/{tid:[0-9a-z]+}', function (Request $request, Response $response, $args) {
            // mark tutor as absent
            $classId = $args['id'];
            $sessionId = $args['sid'];
            $tutorId = $args['tid'];
            $result = DBClass::markAbsent($classId, $sessionId, $tutorId);
            if (!$result) {
                return $response->withStatus(400);
            }
            return $response->withStatus(200);
        })->add($authMiddleware)->add($leaderAboveMiddleware);
        $subgroup->options('/absent/{tid:[0-9a-z]+}', function (Request $request, Response $response, $args) {
            return $response->withStatus(200);
        });

        $subgroup->put('/addexternal/{tid:[0-9a-z]+}', function (Request $request, Response $response, $args) {
            // add a tutor that is not under this class to this session
            return $response;
        })->add($authMiddleware)->add($leaderAboveMiddleware);

        $subgroup->put('/removeexternal/{tid:[0-9a-z]+}', function (Request $request, Response $response, $args) {
            // remove a tutor that is not under this class from this session
            return $response;
        })->add($authMiddleware)->add($leaderAboveMiddleware);
    });
});


$errorMiddleware = $app->addErrorMiddleware(true, true, true);
$app->setBasePath('/backend');
$app->run();
