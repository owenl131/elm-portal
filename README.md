# elm-portal [![Build Status](https://travis-ci.com/owenl131/elm-portal.svg?branch=master)](https://travis-ci.com/owenl131/elm-portal)

## Plan

~~v 0.0 - Implement basic functionality like searching, viewing, update for users, attaching users to classes, and taking attendance for classes.~~

v 0.1 - ~~Implement basic styling,~~ implement extended fields ~~and updating fields, implement home shortcuts, implement create new class, new tutor, display more details on tutor page,~~ try implementing some graphs, implement CIP Hours.

v 0.2 - Implement reports and visualizations.

v 0.3 - Implement proper backend (~~Symphony/Zend/CodeIgniter?~~ SlimFramework)

v 0.4 - Implement same functionality for students, implement other admin functions.

v 0.5 - Animations.

## Goals

Reactive and user-friendly interface.

## Run Backend

~~Testing backend is a simple python flask server, running on `localhost:5000`.~~
Testing backend is now a PHP server running Slim Framework, which connects to a MongoDB instance (credentials separate from codebase). 

```
cd backend
php -S localhost:8001
```

## Run frontend

```
cd frontend
elm reactor
```

Visit `localhost:8000/src/Main.elm`
