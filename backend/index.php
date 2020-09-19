<?php

use Psr\Http\Message\ServerRequestInterface as Request;
use Psr\Http\Message\ResponseInterface as Response;
use Psr\Http\Server\RequestHandlerInterface as RequestHandler;
use Slim\Factory\AppFactory;

require __DIR__ . '/../../../../vendor/autoload.php';

$app = AppFactory::create();
$app->addRoutingMiddleware();

$authMiddleware = function (Request $request, RequestHandler $handler) {
    $response = $handler->handle($request);
    return $response;
};
$adminOnlyMiddleware = function (Request $request, RequestHandler $handler) {
    $response = $handler->handle($request);
    return $response;
};

$app->get('/tutors', function (Request $request, Response $response, $args) {
    // gets list of tutors
    return $response;
})->add($authMiddleware);

$app->group('/tutor/{id:[0-9a-z]+}', function (RouteCollectorProxy $group) {
    $group->get('', function (Request $request, Response $response, $args) {
        // get tutor details
        return $response;
    });
    $group->patch('', function (Request $request, Response $response, $args) {
        // update tutor details
        return $response;
    });
    $group->get('/classes', function (Request $request, Response $response, $args) {
        // get tutor classes
        return $response;
    });
})->add($authMiddleware);

$app->get('/classes', function (Request $request, Response $response, $args) {
})->add($authMiddleware);

$app->group('/class/{id:[0-9]+}', function (RouteCollectorProxy $group) {
    $group->get('', function (Request $request, Response $response, $args) {
        return $response;
    });
    $group->patch('', function (Request $request, Response $response, $args) {
        return $response;
    });
    $group->post('/addtutor/{tid:[0-9a-z]+}', function (Request $request, Response $response, $args) {
        return $response;
    });
    $group->group('/session/{sid:[0-9]+}', function (RouteCollectorProxy $subgroup) {
        $subgroup->get('', function (Request $request, Response $response, $args) {
            // get session details
            return $response;
        });
        $subgroup->patch('', function (Request $request, Response $response, $args) {
            return $response;
        });
        $subgroup->get('/tutors', function (Request $request, Response $response, $args) {
            return $response;
        });
        $subgroup->get('/present', function (Request $request, Response $response, $args) {
            return $response;
        });
        $subgroup->get('/absent', function (Request $request, Response $response, $args) {
            return $response;
        });
        $subgroup->put('/present/{tid:[0-9a-z]+}', function (Request $request, Response $response, $args) {
            return $response;
        });
        $subgroup->put('/absent/{tid:[0-9a-z]+}', function (Request $request, Response $response, $args) {
            return $response;
        });
    });
})->add($authMiddleware);

$errorMiddleware = $app->addErrorMiddleware(true, true, true);
$app->run();
