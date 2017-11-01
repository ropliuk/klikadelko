otwarteWiersze = {}

shinyjs.otwartoModalWiersza = function(params) {
  var nazwa = params[0]
  var nazwa2 = params[1]

  console.log('OMW', nazwa);

  // if (!!otwarteWiersze[nazwa]) {
  //   return;
  // }
  // otwarteWiersze[nazwa] = true;
  // modal = document.getElementById(nazwa + '-modalnew');

  console.log('OMW2', nazwa);

  var zegar = setInterval(szukaj, 500);

  function szukaj() {
    var modal = document.getElementById(nazwa + nazwa2);
    console.log('M', nazwa, modal);

    if (!!modal) {
      clearInterval(zegar);
      console.log('O', nazwa, modal);

      reactToClicksUnderRadios(modal);

      modal.addEventListener('transitioncancel', function() {
        console.log('Z', nazwa);
        wyslijZdarzenie('przelicz', nazwa);
      });
    }
  }
};

shinyjs.zmienionoHaslo = function() {
  var nazwa = 'haslo'
  if (!!otwarteWiersze[nazwa]) {
    return;
  }
  otwarteWiersze[nazwa] = true;

  radios = document.getElementById('f.gl.rok');

  reactToClicksUnderRadios(radios.parentElement);
}
