type props = {
  id: option(string),
  value: option(string),
  className: option(string),
  onClick: option(unit => unit),
  onChange: option(unit => unit)
};

let defaultProps = {id: None, value: None, className: None, onClick: None, onChange: None};