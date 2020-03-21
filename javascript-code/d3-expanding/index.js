/*  TODO
 * 1. Multiple levels of lists
 * 2. Input boxes to the right of each item
 * 3. Autofilling percentage inputs
 * 4. 
 *
 */

var data = [
  {
    "name": "Letter",
    "children": [
      ["i", "ii"],
      "b",
      "c"
    ]
  },
  {
    "name": "Number",
    "children": [
      "1",
      "2",
      "3"
    ]
  }
];

var parent = d3.select("ul");
var list = parent.selectAll("li")
  .data(data)
  .enter()
  .append("li")
  .text(function (d, i) {
    return d.name;
  })
  .on("click", expand);

function expand() {
    d3.select(this)
      .on("click", collapse)
      .append("ul")
      .selectAll("li")
      .data(function(d, i) {return d.children; })
      .enter()
      .append("li")
      .text(function (d, i) {return d; });
}

function collapse() {
  d3.select(this)
    .on("click", expand)
    .select("ul")
    .remove();
}
