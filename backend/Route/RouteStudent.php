<?php

use Psr\Http\Message\ServerRequestInterface as Request;
use Slim\Http\Response as Response;
use Slim\Routing\RouteCollectorProxy;

require_once 'Model/MTutor.php';
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
        $response->withStatus(400, $e->getMessage());
    }
};
