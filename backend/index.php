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
require 'Model/MTutor.php';
require 'Model/MClass.php';
require 'Model/MClassTutor.php';
require 'Model/MClassSession.php';

require __DIR__ . '/../vendor/autoload.php';


$app = AppFactory::create();
$app->add(function (Request $request, RequestHandler $handler) {
    $routeContext = RouteContext::fromRequest($request);
    $routingResults = $routeContext->getRoutingResults();
    $methods = $routingResults->getAllowedMethods();
    $requestHeaders = $request->getHeaderLine('Access-Control-Request-Headers');
    $response = $handler->handle($request);
    $response = $response
        ->withHeader('Access-Control-Allow-Origin', '*')
        ->withHeader('Access-Control-Allow-Methods', implode(', ', $methods))
        ->withHeader('Access-Control-Allow-Headers', $requestHeaders ?: '*');

    $response = $response->withHeader('Access-Control-Allow-Credentials', 'true');
    return $response;
});
$app->addRoutingMiddleware();

function getDB(): MongoDB\Database
{
    return (new MongoDB\Client(connect_string()))->selectDatabase('elmportal1');
}

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
        $db = getDB();
        $result = MTutor::retrieveBySessionId($db, $sessionId);
        if ($result != null) {
            $request = $request
                ->withAttribute('sessionId', $sessionId)
                ->withAttribute('tutorId', $result->id);
            $response = $handler->handle($request);
            return $response;
        }
        error_log("Auth with session id failed");
        return (new ResponseFactory)->createResponse(401);
    }
    error_log("Authorization header not matched");
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

$app->options('/', function (Request $request, Response $response, $args) {
    return $response->withStatus(200);
});

/**
 * Log in using email and password, returns session id
 */
$app->post('/auth', function (Request $request, Response $response, $args) {
    $parsedBody = $request->getParsedBody();
    $email = $parsedBody['email'];
    $password = $parsedBody['password'];
    $db = getDB();
    $result = MTutor::retrieveByCredentials($db, $email, $password);
    if ($result == null) {
        error_log("Auth with credentials failed: " . $email);
        return $response->withStatus(401);
    }
    return $response->withJson(
        array(
            'email' => $email,
            'session' => $result->sessionId,
            'sessionExpiry' => $result->sessionExpiry->getTimestamp()
        ),
        200
    );
});
$app->options('/auth', function (Request $request, Response $response, $args) {
    return $response->withStatus(200);
});

/**
 * Gets tutor list
 */
$app->get('/tutors', function (Request $request, Response $response, $args) {
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
})->add($authMiddleware);

$app->options('/tutors', function (Request $request, Response $response, $args) {
    return $response->withStatus(200);
});

/**
 * Post new tutor
 */
$app->post('/tutors/new', function (Request $request, Response $response, $args) {
    $body = $request->getParsedBody();
    $db = getDB();
    try {
        $tutor = MTutor::create($db, $body);
        return $response->withJson(array('id' => $tutor->id), 200);
    } catch (Exception $e) {
        $response->withStatus(400, $e->getMessage());
    }
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
});


$app->group('/tutor/{id:[0-9a-z]+}', function (RouteCollectorProxy $group) use ($authMiddleware, $adminOnlyMiddleware) {

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
    $db = getDB();
    $classes = MClass::retrieveMany($db, $page, $queryParams);
    $classes['data'] = array_map(function (MClass $elem) {
        return $elem->toAssoc();
    }, $classes['data']);
    return $response->withJson($classes, 200);
})->add($authMiddleware);

$app->options('/classes', function (Request $request, Response $response, $args) {
    return $response->withStatus(200);
});

/**
 * Post new class
 */
$app->post('/classes/new', function (Request $request, Response $response, $args) {
    $body = $request->getParsedBody();
    $db = getDB();
    $result = MClass::create($db, $body);
    return $response->withJson(array('id' => $result->id), 200);
});

$app->options('/classes/new', function (Request $request, Response $response, $args) {
    return $response->withStatus(200);
});

/**
 * Operations on a class
 */
$app->group('/class/{id:[a-z0-9]+}', function (RouteCollectorProxy $group) use ($authMiddleware, $adminOnlyMiddleware, $leaderAboveMiddleware) {

    $group->get('', function (Request $request, Response $response, $args) {
        // get class details
        $classId = $args['id'];
        $db = getDB();
        $class = MClass::retrieve($db, $classId);
        return $response->withJson($class->toAssoc(), 200);
    })->add($authMiddleware);
    $group->options('', function (Request $request, Response $response, $args) {
        return $response->withStatus(200);
    });

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
    $group->options('/tutors', function (Request $request, Response $response, $args) {
        return $response->withStatus(200);
    });

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

    $group->options('/sessions', function (Request $request, Response $response, $args) {
        return $response->withStatus(200);
    });

    $group->post('/addsession', function (Request $request, Response $response, $args) {
        $db = getDB();
        $body = $request->getParsedBody();
        $classId = $args['id'];
        $class = MClass::retrieve($db, $classId);
        $sess = $class->addSession($body);
        return $response->withJson(array('id' => $sess->id), 200);
    })->add($authMiddleware)->add($adminOnlyMiddleware);

    $group->options('/addsession', function (Request $request, Response $response, $args) {
        return $response->withStatus(200);
    });

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
// $app->setBasePath('/backend');
$app->run();
