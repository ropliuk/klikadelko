disableNonselChildren = (x) => {
  for (let y of x.querySelectorAll('input[type=radio]')) {
    for (let z of y.parentElement.querySelectorAll('.shiny-bound-input')) {
      z.disabled = !y.checked;
    }
  }
};

setDisableNonselChildren = (elem) => {
  for (let x of elem.querySelectorAll(".shiny-options-group")) {
    setTimeout(() => disableNonselChildren(x), 500)
    x.addEventListener('change', () => disableNonselChildren(x))
  }
};

selectRadioOption = (radioGroup, option) => {
  radioGroup.value = option.value;
  Shiny.onInputChange(radioGroup.id, option.value);
  option.checked = true;
};

reactToClicksUnderRadios = (elem) => {
  for (let radioGroup of elem.querySelectorAll(".shiny-input-radiogroup")) {
    for (let option of radioGroup.querySelectorAll('input[type=radio]')) {
      for (let subInput of option.parentElement.querySelectorAll('.shiny-bound-input, .selectize-input')) {
        subInput.addEventListener('click', () => selectRadioOption(radioGroup, option));
      }
    }
  }
};
