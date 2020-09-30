<?php

use Psr\Http\Message\ServerRequestInterface as Request;
use Psr\Http\Message\ResponseInterface as Response;
use Psr\Http\Server\RequestHandlerInterface as RequestHandler;
use Slim\Factory\AppFactory;

require __DIR__ . '/../vendor/autoload.php';

$app = AppFactory::create();
$app->addRoutingMiddleware();

$authMiddleware = function (Request $request, RequestHandler $handler) {
    // authenticates user
    $response = $handler->handle($request);
    return $response;
};

$leaderAboveMiddleware = function (Request $request, RequestHandler $handler) {
    $response = $handler->handle($request);
    return $response;
};

$adminOnlyMiddleware = function (Request $request, RequestHandler $handler) {
    // insert for functions requiring admin level rights
    $response = $handler->handle($request);
    return $response;
};

$app->get('/tutors', function (Request $request, Response $response, $args) {
    // gets list of tutors
    return $response;
})->add($authMiddleware);

$app->get('/tutorstats', function (Request $request, Response $response, $args) {
    // returns summary stats by age, school, commencement date, school type, languages spoken, available day
    return $response;
})->add($authMiddleware);

$app->group('/my', function (RouteCollectorProxy $group) {
    // for actions for currently logged in user

    $group->get('/details', function (Request $request, Response $response, $args) {
        // get own profile
        return $response;
    });
    $group->patch('/details', function (Request $request, Response $response, $args) {
        // update own profile
        return $response;
    });
    $group->get('/classes', function (Request $request, Response $response, $args) {
        // get own classes (for home page)
        return $response;
    });
})->add($authMiddleware);

$app->group('/tutor/{id:[0-9a-z]+}', function (RouteCollectorProxy $group) use ($adminOnlyMiddleware) {
    $group->get('', function (Request $request, Response $response, $args) {
        // get tutor details
        return $response;
    });
    $group->patch('', function (Request $request, Response $response, $args) {
        // update tutor details
        return $response;
    })->add($adminOnlyMiddleware);
    $group->get('/classes', function (Request $request, Response $response, $args) {
        // get tutor classes
        return $response;
    });
})->add($authMiddleware);

$app->get('/classes', function (Request $request, Response $response, $args) {
    // get list of classes, subject to filters
    return $response;
})->add($authMiddleware);

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
$app->run();
