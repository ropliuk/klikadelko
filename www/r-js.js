flagiZdarzen = {};

wyslijZdarzenie = (nazwa, parametr) => {
  console.log('WZ', nazwa, !flagiZdarzen[nazwa]);
  Shiny.onInputChange('event.param.' + nazwa, [parametr || '', Math.random()]);
  Shiny.onInputChange('event.' + nazwa, [!flagiZdarzen[nazwa], Math.random()]);
  flagiZdarzen[nazwa] = !flagiZdarzen[nazwa];
}
