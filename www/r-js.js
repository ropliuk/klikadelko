flagiZdarzen = {};

noweZdarzenie = (nazwa) => {
  Shiny.addCustomMessageHandler('on_' + nazwa, wartosc => {
    console.log('FZ', nazwa, wartosc);
    flagiZdarzen[nazwa] = wartosc;
  });
}

wyslijZdarzenie = (nazwa) => {
  console.log('WZ', nazwa, !flagiZdarzen[nazwa]);
  Shiny.onInputChange('event.' + nazwa, !flagiZdarzen[nazwa]);
}
