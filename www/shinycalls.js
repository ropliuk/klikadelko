otwarteWiersze = {}

shinyjs.otwartoModalWiersza = function(params) {
  nazwa = params[0]
  nazwa2 = params[1]

  if (!!otwarteWiersze[nazwa]) {
    return;
  }
  otwarteWiersze[nazwa] = true;
  // modal = document.getElementById(nazwa + '-modalnew');
  modal = document.getElementById(nazwa + nazwa2);

  console.log('O', nazwa, modal);

  reactToClicksUnderRadios(modal);

  modal.addEventListener('transitioncancel', function() {
    console.log('Z', nazwa);
    wyslijZdarzenie('przelicz');
  });
};

shinyjs.zmienionoHaslo = function() {
  nazwa = 'haslo'
  if (!!otwarteWiersze[nazwa]) {
    return;
  }
  otwarteWiersze[nazwa] = true;

  radios = document.getElementById('f.gl.rok');

  reactToClicksUnderRadios(radios.parentElement);
}
