<?php

use Psr\Http\Message\ServerRequestInterface as Request;
use Psr\Http\Server\RequestHandlerInterface as RequestHandler;
use Slim\Factory\AppFactory;
use Slim\Http\Response as Response;
use Slim\Psr7\Factory\ResponseFactory;
use Slim\Routing\RouteCollectorProxy;

require 'Model/DBTutor.php';
require 'Model/DBClass.php';
require __DIR__ . '/../vendor/autoload.php';


$app = AppFactory::create();
$app->add(function (Request $request, RequestHandler $handler) {
    $response = $handler->handle($request);
    return $response
        ->withHeader('Access-Control-Allow-Origin', '*')
        ->withHeader('Access-Control-Allow-Headers', 'X-Requested-With, Content-Type, Accept, Origin, Authorization')
        ->withHeader('Access-Control-Allow-Methods', 'GET, POST, PUT, DELETE, PATCH, OPTIONS');
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
            $request = $request->withAttribute('sessionId', $sessionId);
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
        unset($elem['sessionId']);
        unset($elem['sessionExpiry']);
        return $elem;
    }, $data['data']);
    $response = $response->withJson($data, 200);
    return $response;
})->add($authMiddleware);

$app->options('/tutors', function (Request $request, Response $response, $args) {
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
        return $response;
    })->add($authMiddleware);

    $group->patch('/details', function (Request $request, Response $response, $args) {
        // update own profile
        return $response;
    })->add($authMiddleware);

    $group->get('/classes', function (Request $request, Response $response, $args) {
        // get own classes (for home page)
        return $response;
    })->add($authMiddleware);
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
        unset($tutorData['sessionId']);
        unset($tutorData['sessionExpiry']);
        return $response->withJson($tutorData, 200);
    })->add($authMiddleware);
    $group->options('', function (Request $request, Response $response, $args) {
        return $response->withStatus(200);
    });

    $group->patch('', function (Request $request, Response $response, $args) {
        // update tutor details
        return $response;
    })->add($authMiddleware)->add($adminOnlyMiddleware);

    $group->get('/classes', function (Request $request, Response $response, $args) {
        // get tutor classes
        return $response;
    })->add($authMiddleware);
});


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


$app->get('/classestoday', function (Request $Request, Response $response, $args) {
    // get all active classes with classes on this weekday with the most recent class at most 2 weeks ago
    return $response;
})->add($authMiddleware);


$app->group('/class/{id:[0-9]+}', function (RouteCollectorProxy $group) use ($adminOnlyMiddleware, $leaderAboveMiddleware) {

    $group->get('', function (Request $request, Response $response, $args) {
        // get class details
        return $response;
    });

    $group->patch('', function (Request $request, Response $response, $args) {
        // update class details
        return $response;
    })->add($adminOnlyMiddleware);

    $group->post('/addtutor/{tid:[0-9a-z]+}', function (Request $request, Response $response, $args) {
        // add tutor to class
        return $response;
    })->add($adminOnlyMiddleware);

    $group->put('/updatetutor/{tid:[0-9a-z]+}', function (Request $request, Response $response, $args) {
        // used to edit join date or leave date
        return $response;
    })->add($adminOnlyMiddleware);

    $group->put('/removetutor/{tid:[0-9a-z]+}', function (Request $request, Response $response, $args) {
        // used to delete tutor from class, clears attendance records
        return $response;
    })->add($adminOnlyMiddleware);


    $group->group('/session/{sid:[0-9]+}', function (RouteCollectorProxy $subgroup) use ($leaderAboveMiddleware) {

        $subgroup->get('', function (Request $request, Response $response, $args) {
            // get session details
            return $response;
        });

        $subgroup->patch('', function (Request $request, Response $response, $args) {
            // update session details
            return $response;
        })->add($leaderAboveMiddleware);

        $subgroup->get('/tutors', function (Request $request, Response $response, $args) {
            // get list of tutors in session
            return $response;
        });

        $subgroup->get('/present', function (Request $request, Response $response, $args) {
            // get list of tutors present in session
            return $response;
        });

        $subgroup->get('/absent', function (Request $request, Response $response, $args) {
            // get list of tutors absent in session
            return $response;
        });

        $subgroup->put('/present/{tid:[0-9a-z]+}', function (Request $request, Response $response, $args) {
            // mark tutor as present
            return $response;
        })->add($leaderAboveMiddleware);

        $subgroup->put('/absent/{tid:[0-9a-z]+}', function (Request $request, Response $response, $args) {
            // mark tutor as absent
            return $response;
        })->add($leaderAboveMiddleware);

        $subgroup->put('/addexternal/{tid:[0-9a-z]+}', function (Request $request, Response $response, $args) {
            // add a tutor that is not under this class to this session
            return $response;
        })->add($leaderAboveMiddleware);

        $subgroup->put('/removeexternal/{tid:[0-9a-z]+}', function (Request $request, Response $response, $args) {
            // remove a tutor that is not under this class from this session
            return $response;
        })->add($leaderAboveMiddleware);
    });
})->add($authMiddleware);


$errorMiddleware = $app->addErrorMiddleware(true, true, true);
$app->setBasePath('/backend');
$app->run();
