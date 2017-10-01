flagiZdarzen = {};

wyslijZdarzenie = (nazwa) => {
  console.log('WZ', nazwa, !flagiZdarzen[nazwa]);
  Shiny.onInputChange('event.' + nazwa, [!flagiZdarzen[nazwa], Math.random()]);
  flagiZdarzen[nazwa] = !flagiZdarzen[nazwa];
}
