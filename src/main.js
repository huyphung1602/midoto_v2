import { Elm } from './Main.elm'

var storedState = localStorage.getItem('todo-app-save');
// console.log("Retrieved state: ", storedState);
var startingState = storedState ? storedState : null;

var app = Elm.Main.init({
  node: document.querySelector('main'),
  flags: startingState,
});

app.ports.storeTodos.subscribe(function(todos) {
  if (todos.length > 0) {
    var todosJson = JSON.stringify(todos);
    localStorage.setItem('todo-app-save', todosJson);
    // console.log("Saved state: ", todosJson);
  }
});