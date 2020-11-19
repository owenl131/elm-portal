<?php

use Psr\Http\Message\ServerRequestInterface as Request;
use Psr\Http\Server\RequestHandlerInterface as RequestHandler;
use Slim\Factory\AppFactory;
use Slim\Http\Response as Response;
use Slim\Psr7\Factory\ResponseFactory;
use Slim\Routing\RouteContext;

require_once 'Model/MTutor.php';
require_once 'Model/MClass.php';
require_once 'Model/MClassTutor.php';
require_once 'Model/MClassSession.php';

require_once 'Route/RouteTutor.php';
require_once 'Route/RouteClass.php';
require_once 'Route/RouteUser.php';
require_once 'Route/RouteStudent.php';

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

$respondWithSuccess = function (Request $request, Response $response, $args) {
    return $response->withStatus(200);
};


$app->get('/', function (Request $request, Response $response, $args) {
    $response->getBody()->write('Hello world!');
    return $response;
});

$app->options('/', $respondWithSuccess);

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
$app->options('/auth', $respondWithSuccess);

/**
 * Gets tutor list
 */
$app->get('/tutors', $handleGetTutors)->add($authMiddleware);
$app->options('/tutors', $respondWithSuccess);

/**
 * Post new tutor
 */
$app->post('/tutors/new', $handleNewTutor)->add($authMiddleware);
$app->options('/tutors/new', $respondWithSuccess);

/**
 * Display demographics
 */
$app->get('/tutorstats', function (Request $request, Response $response, $args) {
    // returns summary stats by age, school, commencement date, school type, languages spoken, available day
    // TODO implement
    return $response;
})->add($authMiddleware);
$app->options('/tutorstats', $respondWithSuccess);

/**
 * Functions for the current user
 */
$app->group('/my', getUserRoutes($authMiddleware));

/**
 * Tutor functions
 */
$app->group('/tutor/{id:[0-9a-z]+}', getTutorRoutes($authMiddleware, $adminOnlyMiddleware));

/**
 * Get class list
 */
$app->get('/classes', $handleGetClasses)->add($authMiddleware);
$app->options('/classes', $respondWithSuccess);

/**
 * Post new class
 */
$app->post('/classes/new', $handleNewClass)->add($authMiddleware);
$app->options('/classes/new', $respondWithSuccess);

/**
 * Operations on a class
 */
$app->group('/class/{id:[a-z0-9]+}', getClassRoutes($authMiddleware, $adminOnlyMiddleware, $leaderAboveMiddleware));

$errorMiddleware = $app->addErrorMiddleware(true, true, true);
// $app->setBasePath('/backend');
$app->run();
