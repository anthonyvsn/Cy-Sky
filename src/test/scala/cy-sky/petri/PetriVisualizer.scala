package cysky.petri

import java.nio.file.{Files, Paths}
import java.nio.charset.StandardCharsets

object PetriVisualizer {

  private def selfLoops(m: PetriModule): Set[(Int, Int)] =
    (for {
      i <- m.places.indices
      j <- m.transitions.indices
      if m.pre(i)(j) > 0 && m.post(i)(j) > 0
    } yield (i, j)).toSet

  // ============================================================
  //  GRAPHVIZ DOT
  // ============================================================
  def saveDot(m: PetriModule, filename: String, title: String): Unit = {
    val sb = new StringBuilder
    val loops = selfLoops(m)
    sb.append(s"""digraph "$title" {\n  rankdir=TB;\n  node [fontname="Helvetica",fontsize=11];\n  edge [fontname="Helvetica",fontsize=9];\n\n""")

    m.places.zipWithIndex.foreach { case (p, i) =>
      val tok = m.marking(i)
      val dots = if (tok > 0 && tok <= 5) "\\n" + ("* " * tok).trim else if (tok > 0) s"\\n[$tok]" else ""
      val col = placeColor(p)
      sb.append(s"""  "$p" [shape=circle,style=filled,fillcolor="${col._1}",fontcolor="${col._3}",label="$p$dots",width=1.1];\n""")
    }
    sb.append("\n")

    m.transitions.foreach { t =>
      val col = transColor(t)
      sb.append(s"""  "$t" [shape=box,style="filled,rounded",fillcolor="${col._1}",fontcolor="${col._3}",height=0.4,width=1.4];\n""")
    }
    sb.append("\n")

    m.pre.zipWithIndex.foreach { case (row, i) =>
      row.zipWithIndex.foreach { case (w, j) =>
        if (w > 0 && !loops.contains((i, j))) {
          val lbl = if (w > 1) s""" [label="$w"]""" else ""
          sb.append(s"""  "${m.places(i)}" -> "${m.transitions(j)}"$lbl;\n""")
        }
      }
    }

    m.post.zipWithIndex.foreach { case (row, i) =>
      row.zipWithIndex.foreach { case (w, j) =>
        if (w > 0 && !loops.contains((i, j))) {
          val lbl = if (w > 1) s""" [label="$w"]""" else ""
          sb.append(s"""  "${m.transitions(j)}" -> "${m.places(i)}"$lbl;\n""")
        }
      }
    }

    loops.foreach { case (i, j) =>
      sb.append(s"""  "${m.places(i)}" -> "${m.transitions(j)}" [dir=both,label="self-loop",style=dashed,color="#7F77DD",fontcolor="#534AB7"];\n""")
    }

    sb.append("}\n")
    Files.write(Paths.get(filename), sb.toString.getBytes(StandardCharsets.UTF_8))
    println(s"[DOT] $filename")
  }

  // ============================================================
  //  HTML — layout hierarchique deterministe
  // ============================================================
  def saveHtml(m: PetriModule, filename: String, title: String): Unit = {
    val loops = selfLoops(m)
    val nP = m.places.length
    val nT = m.transitions.length
    val totalNodes = nP + nT

    // --- Calcul des couches (BFS depuis les places avec jetons) ---
    // Chaque noeud : "p0","p1",...,"t0","t1",...
    val visited = scala.collection.mutable.Set[String]()
    val layers  = scala.collection.mutable.ArrayBuffer[Vector[String]]()

    // Couche 0 : places avec marquage initial > 0
    val seeds = m.places.indices.filter(i => m.marking(i) > 0).map(i => s"p$i").toVector
    if (seeds.nonEmpty) {
      layers += seeds
      visited ++= seeds
    }

    var changed = true
    while (changed) {
      changed = false
      val lastLayer = layers.last

      // Si la derniere couche contient des places -> trouver les transitions connectees
      val placesInLast  = lastLayer.filter(_.startsWith("p")).map(_.drop(1).toInt)
      val transInLast   = lastLayer.filter(_.startsWith("t")).map(_.drop(1).toInt)

      if (placesInLast.nonEmpty) {
        // Transitions qui consomment depuis ces places (pre > 0)
        val nextTrans = (for {
          i <- placesInLast
          j <- m.transitions.indices
          if m.pre(i)(j) > 0 && !visited.contains(s"t$j")
        } yield s"t$j").distinct.toVector

        if (nextTrans.nonEmpty) {
          layers += nextTrans
          visited ++= nextTrans
          changed = true
        }
      }

      if (transInLast.nonEmpty || (changed && layers.last.exists(_.startsWith("t")))) {
        val tIdx = if (changed) layers.last.filter(_.startsWith("t")).map(_.drop(1).toInt)
                   else transInLast
        // Places produites par ces transitions (post > 0)
        val nextPlaces = (for {
          j <- tIdx
          i <- m.places.indices
          if m.post(i)(j) > 0 && !visited.contains(s"p$i")
        } yield s"p$i").distinct.toVector

        if (nextPlaces.nonEmpty) {
          layers += nextPlaces
          visited ++= nextPlaces
          changed = true
        }
      }
    }

    // Ajouter les noeuds non visites dans une couche finale
    val remaining = (m.places.indices.map(i => s"p$i") ++ m.transitions.indices.map(j => s"t$j"))
      .filterNot(visited.contains).toVector
    if (remaining.nonEmpty) layers += remaining

    // --- Calcul des positions (x, y) ---
    val PADDING_X = 60
    val PADDING_Y = 50
    val LAYER_GAP = 90
    val NODE_R = 24
    val TRANS_W = 10
    val TRANS_H = 28

    val positions = scala.collection.mutable.Map[String, (Double, Double)]()

    layers.zipWithIndex.foreach { case (layer, li) =>
      val y = PADDING_Y + li * LAYER_GAP
      val n = layer.length
      val totalWidth = 680.0 - 2 * PADDING_X
      val gap = if (n > 1) totalWidth / (n - 1) else 0.0
      val startX = if (n > 1) PADDING_X.toDouble else 340.0

      layer.zipWithIndex.foreach { case (nodeId, ni) =>
        val x = startX + ni * gap
        positions(nodeId) = (x, y)
      }
    }

    val maxY = if (positions.nonEmpty) positions.values.map(_._2).max + LAYER_GAP else 500
    val canvasH = (maxY + 60).toInt

    // --- JSON des noeuds ---
    val nodesJson = {
      val ps = m.places.zipWithIndex.map { case (p, i) =>
        val col = placeColor(p)
        val (px, py) = positions.getOrElse(s"p$i", (340.0, 50.0))
        s"""{"id":"p$i","label":"$p","tokens":${m.marking(i)},"type":"place","x":$px,"y":$py,"fill":"${col._1}","stroke":"${col._2}","text":"${col._3}"}"""
      }
      val ts = m.transitions.zipWithIndex.map { case (t, j) =>
        val col = transColor(t)
        val (tx, ty) = positions.getOrElse(s"t$j", (340.0, 100.0))
        s"""{"id":"t$j","label":"$t","tokens":-1,"type":"trans","x":$tx,"y":$ty,"fill":"${col._1}","stroke":"${col._2}","text":"${col._3}"}"""
      }
      (ps ++ ts).mkString("[", ",", "]")
    }

    // --- JSON des arcs ---
    val edgesBuf = scala.collection.mutable.ArrayBuffer[String]()

    m.pre.zipWithIndex.foreach { case (row, i) =>
      row.zipWithIndex.foreach { case (w, j) =>
        if (w > 0 && !loops.contains((i, j)))
          edgesBuf += s"""{"from":"p$i","to":"t$j","w":$w,"loop":false}"""
      }
    }
    m.post.zipWithIndex.foreach { case (row, i) =>
      row.zipWithIndex.foreach { case (w, j) =>
        if (w > 0 && !loops.contains((i, j)))
          edgesBuf += s"""{"from":"t$j","to":"p$i","w":$w,"loop":false}"""
      }
    }
    loops.foreach { case (i, j) =>
      edgesBuf += s"""{"from":"p$i","to":"t$j","w":${m.pre(i)(j)},"loop":true}"""
    }
    val edgesJson = edgesBuf.mkString("[", ",", "]")

    // --- Matrices HTML ---
    def matHtml(name: String, mat: Vector[Vector[Int]]): String = {
      val hdr = "<tr><th></th>" + m.transitions.zipWithIndex.map { case (t, j) =>
        s"<th class='th'>T$j<br><small>$t</small></th>"
      }.mkString + "</tr>"
      val rows = mat.zipWithIndex.map { case (row, i) =>
        "<tr><td class='pl'>P" + i + " " + m.places(i) + " [" + m.marking(i) + "]</td>" +
        row.map(v => if (v != 0) s"<td class='nz'>$v</td>" else "<td>0</td>").mkString + "</tr>"
      }.mkString("\n")
      s"<h3>$name</h3><div class='tw'><table>$hdr\n$rows</table></div>"
    }

    val inc = m.pre.zip(m.post).map { case (a, b) => a.zip(b).map { case (x, y) => y - x } }
    val matAll = matHtml("Matrice Pre", m.pre) + matHtml("Matrice Post", m.post) + matHtml("Matrice C = Post - Pre", inc)
    val markHtml = m.places.zipWithIndex.map { case (p, i) =>
      val t = m.marking(i)
      val c = if (t > 0) " ht" else ""
      s"<span class='mk$c'>$p = $t</span>"
    }.mkString(" ")

    val html = raw"""<!DOCTYPE html>
<html lang="fr"><head><meta charset="UTF-8">
<meta name="viewport" content="width=device-width,initial-scale=1">
<title>$title</title>
<style>
*{margin:0;padding:0;box-sizing:border-box}
body{font-family:system-ui,-apple-system,Helvetica,Arial,sans-serif;background:#0f0f1a;color:#ddd}
.hd{padding:16px 20px 10px;border-bottom:1px solid #252540}
.hd h1{font-size:18px;font-weight:500;color:#64ffda}
.hd p{font-size:12px;color:#777;margin-top:3px}
.tabs{display:flex;border-bottom:1px solid #252540;padding:0 20px}
.tab{padding:9px 18px;font-size:13px;color:#777;cursor:pointer;background:none;border:none;border-bottom:2px solid transparent}
.tab:hover{color:#bbb}
.tab.a{color:#64ffda;border-color:#64ffda}
.pn{display:none}.pn.a{display:block}
canvas{display:block;width:100%;background:#10102a}
.leg{display:flex;gap:14px;padding:6px 20px;font-size:11px;color:#666;flex-wrap:wrap;border-bottom:1px solid #252540}
.leg span{display:flex;align-items:center;gap:3px}
.d{width:11px;height:11px;border-radius:50%;border:1.5px solid;display:inline-block}
.s{width:11px;height:11px;border-radius:2px;border:1.5px solid;display:inline-block}
.mat{padding:20px;overflow-x:auto}
h3{font-size:14px;font-weight:500;color:#64ffda;margin:16px 0 6px}
h3:first-child{margin-top:0}
.tw{overflow-x:auto;margin-bottom:12px}
.tw table{border-collapse:collapse;font-size:11px}
.tw td,.tw th{border:1px solid #252540;padding:3px 5px;text-align:center;white-space:nowrap}
.th{background:#1a1a30;color:#ba7517}
.pl{background:#0c1f14;color:#5dcaa5;text-align:left;position:sticky;left:0}
.nz{color:#64ffda;font-weight:bold}
.mks{margin:6px 0 16px;display:flex;flex-wrap:wrap;gap:5px}
.mk{font-size:11px;padding:3px 8px;border-radius:5px;background:#1a1a30;border:1px solid #252540}
.ht{background:#0c1f14;border-color:#0F6E56;color:#5dcaa5}
.sts{display:flex;gap:10px;padding:12px 20px;flex-wrap:wrap}
.st{background:#1a1a30;border-radius:6px;padding:10px 16px;text-align:center;min-width:80px}
.sv{display:block;font-size:20px;font-weight:500;color:#64ffda}
.sl{display:block;font-size:10px;color:#777;margin-top:2px}
</style>
</head><body>
<div class="hd"><h1>$title</h1>
<p>$nP places | $nT transitions | ${loops.size} self-loops | ${edgesBuf.size} arcs</p></div>
<div class="tabs">
<button class="tab a" onclick="stab('g',this)">Reseau de Petri</button>
<button class="tab" onclick="stab('m',this)">Matrices</button>
</div>
<div class="pn a" id="g">
<div class="leg">
<span><span class="d" style="background:#E1F5EE;border-color:#0F6E56"></span>Place</span>
<span><span class="d" style="background:#EAF3DE;border-color:#3B6D11"></span>Buffer</span>
<span><span class="d" style="background:#E6F1FB;border-color:#185FA5"></span>Open</span>
<span><span class="d" style="background:#FCEBEB;border-color:#A32D2D"></span>Close</span>
<span><span class="s" style="background:#FAEEDA;border-color:#BA7517"></span>Transition</span>
<span style="color:#7F77DD">--- Self-loop</span>
</div>
<canvas id="cv"></canvas>
</div>
<div class="pn" id="m">
<div class="sts">
<div class="st"><span class="sv">$nP</span><span class="sl">places</span></div>
<div class="st"><span class="sv">$nT</span><span class="sl">transitions</span></div>
<div class="st"><span class="sv">${loops.size}</span><span class="sl">self-loops</span></div>
<div class="st"><span class="sv">${edgesBuf.size}</span><span class="sl">arcs</span></div>
</div>
<div class="mat">
<h3>Marquage initial</h3>
<div class="mks">$markHtml</div>
$matAll
</div>
</div>
<script>
function stab(id,btn){
  document.querySelectorAll('.pn').forEach(function(p){p.classList.remove('a')});
  document.querySelectorAll('.tab').forEach(function(t){t.classList.remove('a')});
  document.getElementById(id).classList.add('a');
  btn.classList.add('a');
  if(id==='g') render();
}

var nodes=$nodesJson;
var edges=$edgesJson;
var nodeMap={};
nodes.forEach(function(n){nodeMap[n.id]=n;});

var cv=document.getElementById('cv'),ctx=cv.getContext('2d');
var W,H=$canvasH;
var R=24,TW=10,TH=28;

function initCanvas(){
  W=cv.parentElement.clientWidth;
  var dpr=window.devicePixelRatio||1;
  cv.width=W*dpr;cv.height=H*dpr;
  cv.style.height=H+'px';
  ctx.setTransform(dpr,0,0,dpr,0,0);
}

function scaleX(x){return x/680*W;}
function scaleY(y){return y/680*W;}

function clipEdge(n,tx,ty){
  var dx=tx-n.x,dy=ty-n.y;
  var a=Math.atan2(dy,dx);
  if(n.type==='place'){
    return{x:n.x+(R+2)*Math.cos(a),y:n.y+(R+2)*Math.sin(a)};
  }
  var hw=TW/2+2,hh=TH/2+2;
  var ca=Math.cos(a),sa=Math.sin(a);
  var t=Math.min(hw/(Math.abs(ca)||0.001),hh/(Math.abs(sa)||0.001));
  return{x:n.x+t*ca,y:n.y+t*sa};
}

function drawArr(x1,y1,x2,y2,col,w){
  ctx.beginPath();ctx.strokeStyle=col;ctx.lineWidth=w||0.8;
  ctx.moveTo(scaleX(x1),scaleY(y1));ctx.lineTo(scaleX(x2),scaleY(y2));ctx.stroke();
  var a=Math.atan2(y2-y1,x2-x1),sz=5;
  var ex=scaleX(x2),ey=scaleY(y2);
  ctx.beginPath();ctx.moveTo(ex,ey);
  ctx.lineTo(ex-sz*Math.cos(a-0.35),ey-sz*Math.sin(a-0.35));
  ctx.lineTo(ex-sz*Math.cos(a+0.35),ey-sz*Math.sin(a+0.35));
  ctx.closePath();ctx.fillStyle=col;ctx.fill();
}

function rrect(x,y,w,h,r){
  ctx.beginPath();ctx.moveTo(x+r,y);
  ctx.lineTo(x+w-r,y);ctx.arcTo(x+w,y,x+w,y+r,r);
  ctx.lineTo(x+w,y+h-r);ctx.arcTo(x+w,y+h,x+w-r,y+h,r);
  ctx.lineTo(x+r,y+h);ctx.arcTo(x,y+h,x,y+h-r,r);
  ctx.lineTo(x,y+r);ctx.arcTo(x,y,x+r,y,r);ctx.closePath();
}

function render(){
  initCanvas();
  ctx.clearRect(0,0,W,H);

  edges.forEach(function(e){
    var a=nodeMap[e.from],b=nodeMap[e.to];
    if(!a||!b)return;
    var pa=clipEdge(a,b.x,b.y);
    var pb=clipEdge(b,a.x,a.y);

    if(e.loop){
      var mx=(a.x+b.x)/2,my=(a.y+b.y)/2;
      var nx=-(b.y-a.y)*0.35,ny=(b.x-a.x)*0.35;
      var cx1=mx+nx,cy1=my+ny;
      ctx.beginPath();ctx.strokeStyle='#7F77DD';ctx.lineWidth=0.8;
      ctx.setLineDash([4,3]);
      ctx.moveTo(scaleX(pa.x),scaleY(pa.y));
      ctx.quadraticCurveTo(scaleX(cx1),scaleY(cy1),scaleX(pb.x),scaleY(pb.y));
      ctx.stroke();ctx.setLineDash([]);
      var ae=Math.atan2(pb.y-cy1,pb.x-cx1),sz=4;
      var ex=scaleX(pb.x),ey=scaleY(pb.y);
      ctx.beginPath();ctx.moveTo(ex,ey);
      ctx.lineTo(ex-sz*Math.cos(ae-0.35),ey-sz*Math.sin(ae-0.35));
      ctx.lineTo(ex-sz*Math.cos(ae+0.35),ey-sz*Math.sin(ae+0.35));
      ctx.closePath();ctx.fillStyle='#7F77DD';ctx.fill();
    } else {
      drawArr(pa.x,pa.y,pb.x,pb.y,'#5F5E5A',0.7);
      if(e.w>1){
        var mx2=scaleX((pa.x+pb.x)/2),my2=scaleY((pa.y+pb.y)/2);
        ctx.fillStyle='#BA7517';ctx.font='bold 9px system-ui';ctx.textAlign='center';
        ctx.fillText(e.w.toString(),mx2,my2-4);
      }
    }
  });

  nodes.forEach(function(n){
    var sx=scaleX(n.x),sy=scaleY(n.y);
    ctx.textAlign='center';ctx.textBaseline='middle';

    if(n.type==='place'){
      var sr=R*W/680;
      ctx.beginPath();ctx.arc(sx,sy,sr,0,Math.PI*2);
      ctx.fillStyle=n.fill;ctx.fill();
      ctx.strokeStyle=n.stroke;ctx.lineWidth=1.2;ctx.stroke();
      ctx.fillStyle=n.text;ctx.font='bold 10px system-ui';
      if(n.tokens>0&&n.tokens<=4){
        var dots='';for(var k=0;k<n.tokens;k++)dots+='\u25CF';
        ctx.fillText(dots,sx,sy);
      }else if(n.tokens>4){
        ctx.fillText('['+n.tokens+']',sx,sy);
      }
      ctx.fillStyle='#bbb';ctx.font='8px system-ui';
      ctx.fillText(n.label,sx,sy+sr+10);
    }else{
      var sw=TW*W/680,sh=TH*W/680;
      ctx.fillStyle=n.fill;ctx.strokeStyle=n.stroke;ctx.lineWidth=1.2;
      rrect(sx-sw/2,sy-sh/2,sw,sh,2);ctx.fill();ctx.stroke();
      ctx.fillStyle='#bbb';ctx.font='7px system-ui';
      ctx.fillText(n.label,sx,sy+sh/2+9);
    }
  });
}

render();
window.addEventListener('resize',function(){render();});
</script>
</body></html>"""

    Files.write(Paths.get(filename), html.getBytes(StandardCharsets.UTF_8))
    println(s"[HTML] $filename")
  }

  // === Couleurs ===
  private def placeColor(name: String): (String, String, String) = name match {
    case n if n.startsWith("Buffer")   => ("#EAF3DE", "#3B6D11", "#27500A")
    case n if n.contains("open")       => ("#E6F1FB", "#185FA5", "#0C447C")
    case n if n.contains("close")      => ("#FCEBEB", "#A32D2D", "#791F1F")
    case n if n.contains("count")      => ("#EEEDFE", "#534AB7", "#3C3489")
    case _                             => ("#E1F5EE", "#0F6E56", "#085041")
  }

  private def transColor(name: String): (String, String, String) = name match {
    case n if n.startsWith("lock")     => ("#FCEBEB", "#A32D2D", "#791F1F")
    case n if n.startsWith("unlock")   => ("#E6F1FB", "#185FA5", "#0C447C")
    case n if n.contains("landing")    => ("#EAF3DE", "#3B6D11", "#27500A")
    case n if n.contains("takeoff")    => ("#FAECE7", "#993C1D", "#712B13")
    case n if n.contains("garage")     => ("#E1F5EE", "#0F6E56", "#085041")
    case n if n.contains("redirect")   => ("#EAF3DE", "#639922", "#27500A")
    case n if n.contains("tw") && n.contains("tr") => ("#FAEEDA", "#854F0B", "#633806")
    case _                             => ("#FAEEDA", "#BA7517", "#633806")
  }
}