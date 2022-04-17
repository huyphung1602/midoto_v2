import { Elm } from './Main.elm'
import { Howl } from 'howler';

const storedState = localStorage.getItem('midoto-save');
const startingState = storedState ? JSON.parse(storedState) : null;

var app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: startingState,
});

app.ports.storeTodos.subscribe(function(todos) {
  if (todos.length > 0) {
    const todosJson = JSON.stringify(todos);
    localStorage.setItem('midoto-save', todosJson);
    // console.log("Saved state: ", todosJson);
  }
});

app.ports.ringTheBell.subscribe(function(isEnableBell) {
  if (isEnableBell) {
    const sound = new Howl({
      src: ['https://assets.mixkit.co/sfx/preview/mixkit-clock-bells-hour-signal-1069.mp3'],
      volume: 0.5,
      html5: true,
      onend: function () {
        console.log('Finished');
      },
    });
    sound.play();
  }
});
