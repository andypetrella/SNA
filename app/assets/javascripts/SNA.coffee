#list helpers
find = (list, p) ->
        filtered = list.filter(p)
        if (filtered.length > 0)
          filtered[0]
        else
          undefined

contains = (list, p) -> find(list, p) != undefined

$ ->
  width = 500
  height = 400
  colors = d3.scale.category10();

  window.force = d3.layout.force()
                    .gravity(.05)
                    .distance(50)
                    .charge(-100)
                    .size([width, height])

  svg = d3.select("#graph").append("svg")
                            .attr("width", width)
                            .attr("height", height);

  window.stats = {}


  stats.infected = {}
  stats.infected.margin = [30,30,30,30]
  stats.infected.size = [
                          200-stats.infected.margin[1]-stats.infected.margin[3],
                          150-stats.infected.margin[0]-stats.infected.margin[2]
                        ]
  stats.infected.svg = d3.select("#stats")
                          .append("svg")
                            .attr("width",  stats.infected.size[0]+stats.infected.margin[1]+stats.infected.margin[3])
                            .attr("height", stats.infected.size[1]+stats.infected.margin[0]+stats.infected.margin[2])
                          .append("g")
                            .attr("transform", "translate(" + stats.infected.margin[3] + "," + stats.infected.margin[0] + ")")
  stats.infected.stream = []
  stats.infected.x = {}
  stats.infected.x.scale = (data) -> d3.scale.linear().domain([0,data.length]).range([0, stats.infected.size[0]])
  stats.infected.x.axis = (scale) -> d3.svg.axis().scale(scale)
  stats.infected.y = {}
  stats.infected.y.scale = (data) -> d3.scale.linear().domain(d3.extent(data)).range([stats.infected.size[1], 0])
  stats.infected.y.axis = (scale) -> d3.svg.axis().scale(scale).orient("left")
  stats.infected.line = d3.svg.line()
                                .x((d, i) -> stats.infected.x.scale.memo(i))
                                .y((d)    -> stats.infected.y.scale.memo(d))


  window.reflow = (stat, data) ->
    stat.svg.selectAll("*").remove()

    stats.infected.x.scale.memo = stats.infected.x.scale(data)
    stats.infected.y.scale.memo = stats.infected.y.scale(data)
    stat.svg.append("g").attr("transform", "translate(0," + stat.size[1] + ")").call(stat.x.axis(stat.x.scale(data)))
    stat.svg.append("g").attr("transform", "translate(0, 0)").call(stat.y.axis(stat.y.scale(data)))
    stat.svg.append("path").attr("d", stat.line(data))


  window.force
    .nodes([])
    .links([])
    .start();

  nodes = window.force.nodes()
  links = window.force.links()
  node = svg.selectAll(".node")
  link = svg.selectAll(".link")

  window.force.on("tick", () ->
    link.attr("x1", (d) -> d.source.x)
        .attr("y1", (d) -> d.source.y)
        .attr("x2", (d) -> d.target.x)
        .attr("y2", (d) -> d.target.y)

    node.attr("cx", (c) -> c.x)
        .attr("cy", (c) -> c.y)
        .attr("r", ".35em")
  )

  drawGraph = (dataNodes) ->
    nodes.length = 0
    links.length = 0
    missing = []

    addNode(n, missing) for n in dataNodes

    shouldStayEmpty = []
    for m in missing
      addLink(m[0], m[1], shouldStayEmpty)

    if shouldStayEmpty.length
      console.error("all links should have been added. See below:")
      console.dir(shouldStayEmpty)


  updateWithId = (n) ->
    n.node.id = n.node[0].substring(n.node[0].lastIndexOf('/')+1)


  addNode = (n, missing) ->
    updateWithId(n)
    nodes.push(n)
    if n.links
      for l in n.links
        addLink(n, l, missing)

  addLink = (s, tid, missing) ->
    target = find(nodes,  (_n) -> _n.node.id == ""+tid)
    if target
      links.push({source: s, target: target})
    else
      missing.push([s, tid])

  changeNode = (ns) ->
    if not ns
      return
    if not $.isArray(ns)
      ns = [ns]
    else if ns.length == 0
      return

    updateWithId(n) for n in ns

    previous = nodes.filter((_n) -> contains(ns, (_nn) -> ""+_nn.node.id is ""+_n.node.id))
    nodes = nodes.filter((_n) -> not contains(previous, (_nn) -> ""+_nn.node.id == ""+_n.node.id))

    nodes.push(n) for n in ns

    n.links = find(previous, (p) -> ""+p.node.id is ""+n.node.id ).links for n in ns

    links.length = 0
    shouldStayEmpty = []
    for n in nodes
      if n.links
        addLink(n, l, shouldStayEmpty) for l in n.links

    if shouldStayEmpty.length
      console.error("all links should have been added. See below:")
      console.dir(shouldStayEmpty)

    force.nodes(nodes)
    force.links(links)

  withRestart = (f) ->
    (n) ->
      f(n)
      restart()

      stats.infected.stream.push(force.nodes().reduce(((a,b) -> a + (if b.node[1].infected then 1 else 0)), 0))
      reflow(
        stats.infected,
        stats.infected.stream
      )

  restart = () ->
    link = link.data(links)
    link.exit().remove()
    link.enter().insert("line")
                  .attr("class", "link")

    node = node.data(nodes, (n) -> n.node.id)
    node.attr("class", (n) -> if n.node[1].infected then "infected node" else "node")
    node.exit().remove()
    node.enter()
          .insert("circle")
            .attr("class", (n) -> if n.node[1].infected then "infected node" else "node")
    node.call(window.force.drag)

    window.force.start()



  $("#create").click () ->
    playRoutes.controllers.Application.createNode($("#graphs").val()).ajax(
      success: withRestart (n) -> addNode(n, [])
      failure: (error) -> console.error(error)
    )

  $("#infect").click () ->
    playRoutes.controllers.Application.infection($("#graphs").val()).ajax(
      success: withRestart changeNode
      failure: (error) -> console.error(error)
    )

  $("#expand").click () ->
    playRoutes.controllers.Application.expand($("#graphs").val()).ajax(
      success: withRestart changeNode
      failure: (error) -> console.error(error)
    )

  $("#c-graph").submit(() ->
    form = $(this)
    data = form.serializeArray().reduce( ((a,b) -> a[b["name"]]=b["value"]; return a), {}  )
    playRoutes.controllers.Application.generateRandomGraph(data.name, data.degree, data.numberOfNodes, data.infectionRate).ajax(
      success: (data) -> $("#graphs").append("<option value='"+data+"'>"+data+"</option>")
      error: () -> console.log("Failed to generate graph")
    )
    false
  )

  $("#graphs").change () ->
    me = $(@)
    if not me.val()
      return

    stats.infected.stream = []

    d3.json playRoutes.controllers.Application.getGraph(me.val()).url, withRestart(drawGraph)

