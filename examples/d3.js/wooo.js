wooo = function () {
  var self = {};

  self.interval = 1000;
  self.iLikeIt = true;

  self.init = function () {
    var script = document.createElement("script");
    script.setAttribute("src", "http://mbostock.github.com/d3/d3.js?1.29.1")
      document.getElementsByTagName("head")[0].appendChild(script);
    self.tid = setTimeout(self.wooo,self.interval);
  };

  self.wooo = function () {
    d3.selectAll("p")
      .transition()
      .duration(self.interval)
      .style("color",function () { 
          return "hsl(" + (Math.random()*360) + ",100%,50%)"; 
          });
    if (self.iLikeIt) { self.tid = setTimeout(self.wooo,self.interval); }
  };


  self.stop = function () {
    self.iLikeIt = false;
    clearTimeout(self.tid);
    d3.selectAll("p")
      .transition()
      .duration(self.interval)
      .style("color","black");
  };

  return self;
}();

wooo.init();
