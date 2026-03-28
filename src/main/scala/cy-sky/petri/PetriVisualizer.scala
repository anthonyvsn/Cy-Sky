package cysky.petri

import java.nio.file.{Files, Paths}
import java.nio.charset.StandardCharsets

object PetriVisualizer {

  def toHtml(m: PetriModule, title: String = "CY-SKY Petri Net"): String = {

    val placesJson = m.places.zipWithIndex.map { case (name, i) =>
      val tokens = m.marking(i)
      val cat = if (name.startsWith("Buffer")) "buffer"
                else if (name.contains("open")) "open"
                else if (name.contains("close")) "close"
                else if (name.contains("count") || name.contains("Count")) "count"
                else "default"
      "{\"name\":\"" + name + "\",\"tokens\":" + tokens + ",\"cat\":\"" + cat + "\"}"
    }.mkString("[", ",", "]")

    val transJson = m.transitions.zipWithIndex.map { case (name, j) =>
      val cat = if (name.startsWith("tw") && name.contains("_to_tr")) "link"
                else if (name.startsWith("redirect")) "redirect"
                else if (name.contains("lock") || name.contains("Lock")) "lock"
                else if (name.contains("landing") || name.contains("takeoff")) "flight"
                else "default"
      "{\"name\":\"" + name + "\",\"cat\":\"" + cat + "\"}"
    }.mkString("[", ",", "]")

    val arcsList = scala.collection.mutable.ArrayBuffer[String]()
    for (i <- m.places.indices; j <- m.transitions.indices) {
      val preW = m.pre(i)(j)
      val postW = m.post(i)(j)
      if (preW > 0 && postW > 0) {
        arcsList += "{\"from\":\"P" + i + "\",\"to\":\"T" + j + "\",\"w\":" + preW + ",\"type\":\"selfloop\"}"
      } else if (preW > 0) {
        arcsList += "{\"from\":\"P" + i + "\",\"to\":\"T" + j + "\",\"w\":" + preW + ",\"type\":\"pre\"}"
      }
      if (postW > 0 && preW == 0) {
        arcsList += "{\"from\":\"T" + j + "\",\"to\":\"P" + i + "\",\"w\":" + postW + ",\"type\":\"post\"}"
      }
    }
    val arcsJson = arcsList.mkString("[", ",", "]")

    val css = """<style>
  * { margin:0; padding:0; box-sizing:border-box; }
  body { background:#1a1a2e; color:#e0e0e0; font-family:Consolas,monospace; overflow:hidden; }
  #header { position:fixed; top:0; left:0; right:0; height:48px; background:#16213e;
            display:flex; align-items:center; padding:0 20px; z-index:10;
            border-bottom:1px solid #0f3460; }
  #header h1 { font-size:16px; color:#4fc3f7; }
  #header .stats { margin-left:auto; font-size:12px; color:#888; }
  #canvas { position:fixed; top:48px; left:0; right:0; bottom:0; }
  svg { width:100%; height:100%; }
  .place { cursor:pointer; }
  .place:hover circle { stroke:#fff; stroke-width:3; }
  .trans { cursor:pointer; }
  .trans:hover rect { stroke:#fff; stroke-width:2; }
  .arc { fill:none; stroke-width:1.5; }
  .arc.pre { stroke:#ef5350; }
  .arc.post { stroke:#42a5f5; }
  .arc.selfloop { stroke:#66bb6a; stroke-dasharray:6,3; }
  .token { font-size:11px; fill:#fff; text-anchor:middle; font-weight:bold; pointer-events:none; }
  .pname { font-size:8px; fill:#ccc; text-anchor:middle; pointer-events:none; }
  .tname { font-size:7px; fill:#333; text-anchor:middle; pointer-events:none; }
  #legend { position:fixed; bottom:10px; left:10px; background:#16213e; padding:10px;
            border-radius:6px; font-size:11px; border:1px solid #0f3460; }
  #legend div { margin:3px 0; display:flex; align-items:center; gap:8px; }
  .ldot { width:12px; height:12px; border-radius:50%; display:inline-block; }
  .lsq { width:12px; height:12px; display:inline-block; }
  #tooltip { position:fixed; background:#16213e; border:1px solid #4fc3f7; padding:8px 12px;
             border-radius:4px; font-size:11px; display:none; pointer-events:none; z-index:20; }
</style>"""

    val body = """<div id="header">
  <h1>""" + title + """</h1>
  <div class="stats" id="stats"></div>
</div>
<div id="canvas"><svg id="svg"></svg></div>
<div id="legend">
  <div><span class="ldot" style="background:#4CAF50"></span> Buffer</div>
  <div><span class="ldot" style="background:#FFF9C4;border:1px solid #888"></span> Open</div>
  <div><span class="ldot" style="background:#EF9A9A"></span> Close</div>
  <div><span class="ldot" style="background:#90CAF9"></span> Count</div>
  <div style="margin-top:6px"><span class="lsq" style="background:#FFD54F"></span> TW-TR</div>
  <div><span class="lsq" style="background:#CE93D8"></span> Redirect</div>
  <div><span class="lsq" style="background:#FF8A65"></span> Lock/Unlock</div>
  <div><span class="lsq" style="background:#4FC3F7"></span> Landing/Takeoff</div>
  <div style="margin-top:6px;color:#ef5350">--- Pre arc</div>
  <div style="color:#42a5f5">--- Post arc</div>
  <div style="color:#66bb6a">- - Self-loop</div>
</div>
<div id="tooltip"></div>"""

    val dataScript = "<script>\nvar places = " + placesJson + ";\nvar trans = " + transJson + ";\nvar arcs = " + arcsJson + ";\n"

    // All JS as a raw string to prevent Scala from interpreting \d \n etc
    val jsCode: String = {
      val lines = new StringBuilder
      lines.append("document.getElementById('stats').textContent = ")
      lines.append("places.length + ' places | ' + trans.length + ' transitions | ' + arcs.length + ' arcs';")
      lines.append("\n\n")
      lines.append("var svg = document.getElementById('svg');\n")
      lines.append("var W = window.innerWidth;\n")
      lines.append("var H = window.innerHeight - 48;\n\n")
      lines.append("var NS = 'http://www.w3.org/2000/svg';\n")
      lines.append("function el(tag, attrs) {\n")
      lines.append("  var e = document.createElementNS(NS, tag);\n")
      lines.append("  if (attrs) { for (var k in attrs) { if (attrs.hasOwnProperty(k)) e.setAttribute(k, attrs[k]); } }\n")
      lines.append("  return e;\n")
      lines.append("}\n\n")
      lines.append("var pColors = {buffer:'#4CAF50',open:'#FFF9C4',close:'#EF9A9A',count:'#90CAF9','default':'#C8E6C9'};\n")
      lines.append("var tColors = {link:'#FFD54F',redirect:'#CE93D8',lock:'#FF8A65',flight:'#4FC3F7','default':'#A5D6A7'};\n\n")
      // getGroupV2 - use \\d to produce \d in output
      lines.append("function getGroupV2(name) {\n")
      lines.append("  var m = name.match(/_(\\d+)$/);\n")
      lines.append("  if (!m) return 'global';\n")
      lines.append("  var num = m[1];\n")
      lines.append("  if (name.indexOf('TaxiWay') >= 0 || name.indexOf('TW_') >= 0) return 'TW_' + num;\n")
      lines.append("  if (name.indexOf('Track') >= 0 || name.indexOf('TR_') >= 0) return 'TR_' + num;\n")
      lines.append("  if (name.indexOf('BufferTaxiWay') >= 0) return 'TW_' + num;\n")
      lines.append("  if (name.indexOf('BufferTrack') >= 0) return 'TR_' + num;\n")
      lines.append("  return 'global';\n")
      lines.append("}\n\n")
      lines.append("var groupSet = {};\n")
      lines.append("places.forEach(function(p) { groupSet[getGroupV2(p.name)] = true; });\n")
      lines.append("trans.forEach(function(t) { groupSet[getGroupV2(t.name)] = true; });\n")
      lines.append("var groupList = Object.keys(groupSet).sort();\n\n")
      lines.append("var cx = W / 2, cy = H / 2;\n")
      lines.append("var groupPositions = {};\n")
      lines.append("var nGroups = groupList.length;\n")
      lines.append("var gIdx = 0;\n")
      lines.append("groupList.forEach(function(g) {\n")
      lines.append("  if (g === 'global') { groupPositions[g] = {x:cx, y:cy}; }\n")
      lines.append("  else {\n")
      lines.append("    var angle = (2 * Math.PI * gIdx) / Math.max(nGroups - 1, 1) - Math.PI / 2;\n")
      lines.append("    var radius = Math.min(W, H) * 0.35;\n")
      lines.append("    groupPositions[g] = {x: cx + Math.cos(angle)*radius, y: cy + Math.sin(angle)*radius};\n")
      lines.append("    gIdx++;\n")
      lines.append("  }\n")
      lines.append("});\n\n")
      lines.append("var nodePos = {};\n")
      lines.append("var nodesByGroup = {};\n")
      lines.append("var allNodes = [];\n")
      lines.append("places.forEach(function(p,i) { allNodes.push({id:'P'+i, name:p.name, group:getGroupV2(p.name)}); });\n")
      lines.append("trans.forEach(function(t,j) { allNodes.push({id:'T'+j, name:t.name, group:getGroupV2(t.name)}); });\n\n")
      lines.append("allNodes.forEach(function(n) {\n")
      lines.append("  if (!nodesByGroup[n.group]) nodesByGroup[n.group] = [];\n")
      lines.append("  nodesByGroup[n.group].push(n);\n")
      lines.append("});\n\n")
      lines.append("Object.keys(nodesByGroup).forEach(function(group) {\n")
      lines.append("  var nodes = nodesByGroup[group];\n")
      lines.append("  var gp = groupPositions[group];\n")
      lines.append("  var n = nodes.length;\n")
      lines.append("  var localR = 40 + n * 14;\n")
      lines.append("  nodes.forEach(function(node, i) {\n")
      lines.append("    var angle = (2 * Math.PI * i) / n - Math.PI / 2;\n")
      lines.append("    nodePos[node.id] = {x: gp.x + Math.cos(angle)*localR, y: gp.y + Math.sin(angle)*localR};\n")
      lines.append("  });\n")
      lines.append("});\n\n")
      // Defs
      lines.append("var defs = el('defs');\n")
      lines.append("var arrowTypes = [['pre','#ef5350'],['post','#42a5f5'],['selfloop','#66bb6a']];\n")
      lines.append("arrowTypes.forEach(function(at) {\n")
      lines.append("  var marker = el('marker', {id:'arrow-'+at[0], viewBox:'0 0 10 6', refX:'10', refY:'3',\n")
      lines.append("    markerWidth:'8', markerHeight:'6', orient:'auto-start-reverse'});\n")
      lines.append("  var path = el('path', {d:'M0,0 L10,3 L0,6 Z', fill:at[1]});\n")
      lines.append("  marker.appendChild(path); defs.appendChild(marker);\n")
      lines.append("});\n")
      lines.append("svg.appendChild(defs);\n\n")
      // Arcs
      lines.append("var arcG = el('g');\n")
      lines.append("svg.appendChild(arcG);\n")
      lines.append("arcs.forEach(function(arc) {\n")
      lines.append("  var from = nodePos[arc.from]; var to = nodePos[arc.to];\n")
      lines.append("  if (!from || !to) return;\n")
      lines.append("  var attrs = {x1:from.x, y1:from.y, x2:to.x, y2:to.y, 'class':'arc ' + arc.type};\n")
      lines.append("  if (arc.type !== 'selfloop') attrs['marker-end'] = 'url(#arrow-' + arc.type + ')';\n")
      lines.append("  arcG.appendChild(el('line', attrs));\n")
      lines.append("});\n\n")
      // Places
      lines.append("places.forEach(function(p, i) {\n")
      lines.append("  var pos = nodePos['P' + i]; if (!pos) return;\n")
      lines.append("  var g = el('g', {'class':'place', transform:'translate('+pos.x+','+pos.y+')'});\n")
      lines.append("  g.appendChild(el('circle', {r:20, fill:pColors[p.cat]||pColors['default'], stroke:'#555', 'stroke-width':1.5}));\n")
      lines.append("  var tok = el('text', {'class':'token', y:4}); tok.textContent = p.tokens; g.appendChild(tok);\n")
      lines.append("  var lbl = el('text', {'class':'pname', y:32}); lbl.textContent = p.name; g.appendChild(lbl);\n")
      lines.append("  g.onmouseover = function(e) { showTip(e, 'Place: ' + p.name + ' | Tokens: ' + p.tokens); };\n")
      lines.append("  g.onmouseout = hideTip;\n")
      lines.append("  svg.appendChild(g);\n")
      lines.append("});\n\n")
      // Transitions
      lines.append("trans.forEach(function(t, j) {\n")
      lines.append("  var pos = nodePos['T' + j]; if (!pos) return;\n")
      lines.append("  var g = el('g', {'class':'trans', transform:'translate('+pos.x+','+pos.y+')'});\n")
      lines.append("  g.appendChild(el('rect', {x:-24, y:-10, width:48, height:20, rx:3,\n")
      lines.append("    fill:tColors[t.cat]||tColors['default'], stroke:'#555', 'stroke-width':1}));\n")
      lines.append("  var lbl = el('text', {'class':'tname', y:3}); lbl.textContent = t.name; g.appendChild(lbl);\n")
      lines.append("  g.onmouseover = function(e) { showTip(e, 'Transition: ' + t.name); };\n")
      lines.append("  g.onmouseout = hideTip;\n")
      lines.append("  svg.appendChild(g);\n")
      lines.append("});\n\n")
      // Tooltip
      lines.append("var tipEl = document.getElementById('tooltip');\n")
      lines.append("function showTip(e, text) {\n")
      lines.append("  tipEl.textContent = text; tipEl.style.display = 'block';\n")
      lines.append("  tipEl.style.left = (e.clientX + 15) + 'px'; tipEl.style.top = (e.clientY + 15) + 'px';\n")
      lines.append("}\n")
      lines.append("function hideTip() { tipEl.style.display = 'none'; }\n\n")
      // Pan & Zoom
      lines.append("var vb = {x:0, y:0, w:W, h:H};\n")
      lines.append("svg.setAttribute('viewBox', vb.x+' '+vb.y+' '+vb.w+' '+vb.h);\n")
      lines.append("var panning = false, panStart = {x:0, y:0};\n")
      lines.append("svg.onmousedown = function(e) { panning = true; panStart = {x:e.clientX, y:e.clientY}; };\n")
      lines.append("svg.onmousemove = function(e) {\n")
      lines.append("  if (!panning) return;\n")
      lines.append("  var dx = (e.clientX - panStart.x) * (vb.w / W);\n")
      lines.append("  var dy = (e.clientY - panStart.y) * (vb.h / H);\n")
      lines.append("  vb.x -= dx; vb.y -= dy;\n")
      lines.append("  svg.setAttribute('viewBox', vb.x+' '+vb.y+' '+vb.w+' '+vb.h);\n")
      lines.append("  panStart = {x:e.clientX, y:e.clientY};\n")
      lines.append("};\n")
      lines.append("svg.onmouseup = function() { panning = false; };\n")
      lines.append("svg.onmouseleave = function() { panning = false; };\n")
      lines.append("svg.onwheel = function(e) {\n")
      lines.append("  e.preventDefault();\n")
      lines.append("  var scale = e.deltaY > 0 ? 1.1 : 0.9;\n")
      lines.append("  var mx = vb.x + (e.clientX / W) * vb.w;\n")
      lines.append("  var my = vb.y + ((e.clientY - 48) / H) * vb.h;\n")
      lines.append("  vb.w *= scale; vb.h *= scale;\n")
      lines.append("  vb.x = mx - (e.clientX / W) * vb.w;\n")
      lines.append("  vb.y = my - ((e.clientY - 48) / H) * vb.h;\n")
      lines.append("  svg.setAttribute('viewBox', vb.x+' '+vb.y+' '+vb.w+' '+vb.h);\n")
      lines.append("};\n")
      lines.toString()
    }

    "<!DOCTYPE html>\n<html>\n<head>\n<meta charset=\"utf-8\">\n<title>" + title + "</title>\n" +
    css + "\n</head>\n<body>\n" + body + "\n" + dataScript + jsCode + "\n</script>\n</body>\n</html>"
  }

  def toDot(m: PetriModule, title: String = "PetriNet"): String = {
    val sb = new StringBuilder
    sb.append("digraph \"" + title + "\" {\n")
    sb.append("  rankdir=LR;\n")
    sb.append("  node [fontname=Consolas fontsize=10];\n")
    sb.append("  edge [fontname=Consolas fontsize=8];\n\n")

    m.places.zipWithIndex.foreach { case (name, i) =>
      val tokens = m.marking(i)
      val color = if (name.startsWith("Buffer")) "#4CAF50"
                  else if (name.contains("open")) "#FFF9C4"
                  else if (name.contains("close")) "#EF9A9A"
                  else if (name.contains("count") || name.contains("Count")) "#90CAF9"
                  else "#C8E6C9"
      sb.append("  \"" + name + "\" [shape=circle style=filled fillcolor=\"" + color +
                "\" label=\"" + name + "\\n[" + tokens + "]\" width=1.2];\n")
    }

    sb.append("\n")
    m.transitions.zipWithIndex.foreach { case (name, _) =>
      val color = if (name.startsWith("tw") && name.contains("_to_tr")) "#FFD54F"
                  else if (name.startsWith("redirect")) "#CE93D8"
                  else if (name.contains("lock") || name.contains("Lock")) "#FF8A65"
                  else if (name.contains("landing") || name.contains("takeoff")) "#4FC3F7"
                  else "#A5D6A7"
      sb.append("  \"" + name + "\" [shape=box style=filled fillcolor=\"" + color +
                "\" label=\"" + name + "\" height=0.4];\n")
    }

    sb.append("\n")
    for (i <- m.places.indices; j <- m.transitions.indices) {
      val w = m.pre(i)(j)
      if (w > 0) {
        val label = if (w > 1) " label=\"" + w + "\"" else ""
        val style = if (m.post(i)(j) > 0) " style=dashed color=\"#66BB6A\"" else " color=\"#EF5350\""
        sb.append("  \"" + m.places(i) + "\" -> \"" + m.transitions(j) + "\"[" + label + style + "];\n")
      }
    }

    sb.append("\n")
    for (i <- m.places.indices; j <- m.transitions.indices) {
      val w = m.post(i)(j)
      if (w > 0 && m.pre(i)(j) == 0) {
        val label = if (w > 1) " label=\"" + w + "\"" else ""
        sb.append("  \"" + m.transitions(j) + "\" -> \"" + m.places(i) + "\"[" + label + " color=\"#42A5F5\"];\n")
      }
    }

    sb.append("}\n")
    sb.toString()
  }

  def saveHtml(m: PetriModule, path: String, title: String = "CY-SKY Petri Net"): Unit = {
    Files.write(Paths.get(path), toHtml(m, title).getBytes(StandardCharsets.UTF_8))
    println("[PetriVisualizer] HTML saved to: " + path)
  }

  def saveDot(m: PetriModule, path: String, title: String = "PetriNet"): Unit = {
    Files.write(Paths.get(path), toDot(m, title).getBytes(StandardCharsets.UTF_8))
    println("[PetriVisualizer] DOT saved to: " + path)
  }
}