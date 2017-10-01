otwarteWiersze = {}

shinyjs.otwartoModalWiersza = function(params) {
  nazwa = params[0]

  if (!!otwarteWiersze[nazwa]) {
    return;
  }
  otwarteWiersze[nazwa] = true;
  modal = document.getElementById(nazwa + '-modalnew');
  console.log('O', nazwa, modal);

  reactToClicksUnderRadios(modal);

  modal.addEventListener('transitioncancel', function() {
    console.log('Z', nazwa);
    wyslijZdarzenie('przelicz');
  });
};
