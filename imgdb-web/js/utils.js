dojo.require("dojo.date.locale");

function GetLocalizedDateString(year, month, day)
{
  var d = new Date(year, month, day);
  return dojo.date.locale.format(d, {selector:'date', formatLength:'long'});
}
