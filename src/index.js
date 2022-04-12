import { Elm } from './Main.elm'

var storedState = localStorage.getItem('midoto-save');
var startingState = storedState ? storedState : null;

var app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: startingState,
});

app.ports.storeTodos.subscribe(function(todos) {
  if (todos.length > 0) {
    var todosJson = JSON.stringify(todos);
    localStorage.setItem('midoto-save', todosJson);
    // console.log("Saved state: ", todosJson);
  }
});
