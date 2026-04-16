# -*- coding: utf-8 -*-
"""Génère documentation_petri.pdf à partir de reportlab."""

import math, os, sys
from reportlab.lib.pagesizes import A4
from reportlab.lib.styles import getSampleStyleSheet, ParagraphStyle
from reportlab.lib.units import cm
from reportlab.lib import colors
from reportlab.lib.enums import TA_CENTER, TA_LEFT, TA_JUSTIFY
from reportlab.platypus import (
    SimpleDocTemplate, Paragraph, Spacer, Table, TableStyle,
    PageBreak, HRFlowable, Flowable, Preformatted, KeepTogether,
)
from reportlab.graphics.shapes import (
    Drawing, Circle, Rect, String, Line, Polygon, Path, Group,
)
from reportlab.graphics import renderPDF

# ─── Couleurs ─────────────────────────────────────────────────────────────────
C_PLACE   = colors.HexColor("#90EE90")
C_OPEN    = colors.HexColor("#ADD8E6")
C_CLOSE   = colors.HexColor("#FFB6C1")
C_COUNT   = colors.HexColor("#DDA0DD")
C_TRANS   = colors.HexColor("#FFE4B5")
C_BG_CODE = colors.HexColor("#F5F5F5")
C_BORDER  = colors.HexColor("#CCCCCC")
C_HEAD    = colors.HexColor("#2C3E50")
C_BLUE    = colors.HexColor("#2980B9")
C_ORANGE  = colors.HexColor("#E67E22")
C_GREEN   = colors.HexColor("#27AE60")
C_RED     = colors.HexColor("#E74C3C")
C_PURPLE  = colors.HexColor("#8E44AD")

W, H = A4

# ─── Styles ───────────────────────────────────────────────────────────────────
ss = getSampleStyleSheet()

def S(name, **kw):
    return ParagraphStyle(name, **kw)

sTitle = S("sTitle", fontSize=20, leading=26, textColor=C_HEAD,
           alignment=TA_CENTER, spaceAfter=6)
sSubtitle = S("sSubtitle", fontSize=13, leading=18, textColor=C_BLUE,
              alignment=TA_CENTER, spaceAfter=20)
sH1 = S("sH1", fontSize=14, leading=18, textColor=C_HEAD, spaceBefore=14,
        spaceAfter=6, fontName="Helvetica-Bold")
sH2 = S("sH2", fontSize=12, leading=16, textColor=C_BLUE, spaceBefore=10,
        spaceAfter=4, fontName="Helvetica-Bold")
sH3 = S("sH3", fontSize=11, leading=14, textColor=colors.darkblue, spaceBefore=8,
        spaceAfter=3, fontName="Helvetica-Bold")
sBody = S("sBody", fontSize=10, leading=14, spaceAfter=4, alignment=TA_JUSTIFY)
sBullet = S("sBullet", fontSize=10, leading=13, leftIndent=20, spaceAfter=2)
sCode = S("sCode", fontSize=8.5, leading=12, fontName="Courier",
          backColor=C_BG_CODE, leftIndent=10, rightIndent=10,
          spaceBefore=4, spaceAfter=4, borderPadding=5)
sCenter = S("sCenter", fontSize=10, leading=14, alignment=TA_CENTER, spaceAfter=4)
sCap = S("sCap", fontSize=9, leading=12, alignment=TA_CENTER,
         textColor=colors.grey, spaceAfter=8)
sInfo = S("sInfo", fontSize=9.5, leading=13, backColor=colors.HexColor("#EBF5FB"),
          borderPadding=8, spaceAfter=6)
sWarn = S("sWarn", fontSize=9.5, leading=13, backColor=colors.HexColor("#FEF9E7"),
          borderPadding=8, spaceAfter=6)

# ─── Helpers dessin ───────────────────────────────────────────────────────────
def arrow(d, x1, y1, x2, y2, color=colors.black, w=1.4):
    dx, dy = x2-x1, y2-y1
    length = math.hypot(dx, dy)
    if length < 1:
        return
    ux, uy = dx/length, dy/length
    # shorten so arrow doesn't overlap node
    sx1, sy1 = x1 + ux*18, y1 + uy*18
    sx2, sy2 = x2 - ux*18, y2 - uy*18
    d.add(Line(sx1, sy1, sx2, sy2, strokeColor=color, strokeWidth=w))
    al = 8
    ang = 0.4
    p1x = sx2 - al*(ux*math.cos(ang) - uy*math.sin(ang))
    p1y = sy2 - al*(uy*math.cos(ang) + ux*math.sin(ang))
    p2x = sx2 - al*(ux*math.cos(ang) + uy*math.sin(ang))
    p2y = sy2 - al*(uy*math.cos(ang) - ux*math.sin(ang))
    d.add(Polygon([sx2, sy2, p1x, p1y, p2x, p2y],
                  fillColor=color, strokeColor=color, strokeWidth=0))

def loop_arrow(d, x, y, color=colors.black):
    """Boucle auto sur un nœud."""
    r = 14
    d.add(Circle(x+22, y+22, r, fillColor=colors.transparent,
                 strokeColor=color, strokeWidth=1.4))
    # petite flèche
    d.add(Polygon([x+22+r, y+22, x+22+r-6, y+22+5, x+22+r-6, y+22-5],
                  fillColor=color, strokeColor=color))

def place(d, x, y, label, tokens=0, r=20, fc=C_PLACE):
    d.add(Circle(x, y, r, fillColor=fc, strokeColor=colors.black, strokeWidth=1.5))
    if tokens == 1:
        d.add(Circle(x, y, 4, fillColor=colors.black))
    elif tokens == 2:
        d.add(Circle(x-5, y, 3.5, fillColor=colors.black))
        d.add(Circle(x+5, y, 3.5, fillColor=colors.black))
    lines = label.split("\n")
    for k, l in enumerate(lines):
        d.add(String(x, y - r - 12 - k*10, l,
                     textAnchor="middle", fontSize=8, fillColor=colors.black))

def trans(d, x, y, label, w=12, h=32, fc=C_TRANS):
    d.add(Rect(x-w/2, y-h/2, w, h, fillColor=fc,
               strokeColor=colors.black, strokeWidth=1.5))
    lines = label.split("\n")
    for k, l in enumerate(lines):
        d.add(String(x, y - h/2 - 12 - k*10, l,
                     textAnchor="middle", fontSize=7.5, fillColor=colors.black))

# ─── Diagramme : Cycle verrou ─────────────────────────────────────────────────
def diag_lock():
    d = Drawing(420, 120)
    # Fond
    d.add(Rect(0, 0, 420, 120, fillColor=colors.white, strokeColor=colors.white))
    # Places
    place(d,  60, 70, "open",  tokens=1, fc=C_OPEN)
    place(d, 360, 70, "close", tokens=0, fc=C_CLOSE)
    # Transitions
    trans(d, 155, 70, "lock")
    trans(d, 265, 70, "unlock")
    # Arcs
    arrow(d,  60, 70, 155, 70)  # open -> lock
    arrow(d, 155, 70, 360, 70)  # lock -> close
    arrow(d, 360, 70, 265, 70)  # close -> unlock
    arrow(d, 265, 70,  60, 70)  # unlock -> open
    # Labels arcs
    d.add(String(105, 80, "consume", textAnchor="middle", fontSize=7, fillColor=colors.grey))
    d.add(String(258, 80, "produce", textAnchor="middle", fontSize=7, fillColor=colors.grey))
    d.add(String(313, 55, "consume", textAnchor="middle", fontSize=7, fillColor=colors.grey))
    d.add(String(160, 55, "produce", textAnchor="middle", fontSize=7, fillColor=colors.grey))
    return d

# ─── Diagramme : Module Garage ────────────────────────────────────────────────
def diag_garage():
    d = Drawing(460, 160)
    d.add(Rect(0, 0, 460, 160, fillColor=colors.white, strokeColor=colors.white))
    # places : y repère bas
    BG  = (60,  110); GA  = (190, 110); GO  = (330, 110); GC  = (260, 45)
    LK  = (190,  45); UL  = (110, 110)
    place(d, *BG, "BufferGarage\n[5]", fc=C_PLACE)
    place(d, *GA, "Garage\n[5]", tokens=2, fc=C_PLACE)
    place(d, *GO, "G_open\n[1]",  tokens=1, fc=C_OPEN)
    place(d, *GC, "G_close\n[0]", tokens=0, fc=C_CLOSE)
    trans(d, *LK, "lock\nGarage")
    trans(d, *UL, "unlock\nGarage")
    # lockGarage : GA->LK, GO->LK, LK->GC
    arrow(d, GA[0], GA[1], LK[0], LK[1])
    arrow(d, GO[0], GO[1], LK[0], LK[1])
    arrow(d, LK[0], LK[1], GC[0], GC[1])
    # unlockGarage : GC->UL, UL->BG, UL->GO
    arrow(d, GC[0], GC[1], UL[0], UL[1])
    arrow(d, UL[0], UL[1], BG[0], BG[1])
    arrow(d, UL[0], UL[1], GO[0], GO[1])
    return d

# ─── Diagramme : Module TaxiWay ───────────────────────────────────────────────
def diag_taxiway():
    d = Drawing(460, 170)
    d.add(Rect(0, 0, 460, 170, fillColor=colors.white, strokeColor=colors.white))
    BT=(55,100); TW=(200,100); OP=(345,100); CL=(280,40)
    AD=(200,155); LK=(200,40); UL=(115,70)
    place(d, *BT, "BufferTW\n[5]",   fc=C_PLACE)
    place(d, *TW, "TaxiWay\n[0]",    fc=C_PLACE)
    place(d, *OP, "TW_open\n[1]",  tokens=1, fc=C_OPEN)
    place(d, *CL, "TW_close\n[0]", fc=C_CLOSE)
    trans(d, *AD, "addTaxiWay")
    trans(d, *LK, "lockTW")
    trans(d, *UL, "unlockTW")
    # addTaxiWay: OP->AD (loop), AD->TW
    arrow(d, OP[0], OP[1], AD[0], AD[1])
    arrow(d, AD[0], AD[1], OP[0], OP[1])
    arrow(d, AD[0], AD[1], TW[0], TW[1])
    # lockTW: TW->LK, OP->LK, LK->CL
    arrow(d, TW[0], TW[1], LK[0], LK[1])
    arrow(d, OP[0], OP[1], LK[0], LK[1])
    arrow(d, LK[0], LK[1], CL[0], CL[1])
    # unlockTW: CL->UL, UL->BT, UL->OP
    arrow(d, CL[0], CL[1], UL[0], UL[1])
    arrow(d, UL[0], UL[1], BT[0], BT[1])
    arrow(d, UL[0], UL[1], OP[0], OP[1])
    return d

# ─── Diagramme : Module Track ─────────────────────────────────────────────────
def diag_track():
    d = Drawing(500, 190)
    d.add(Rect(0, 0, 500, 190, fillColor=colors.white, strokeColor=colors.white))
    BT=(55,110); TR=(200,110); OP=(345,110); CL=(280,50)
    LA=(140,165); AO=(260,165); LK=(200,50); UL=(110,80); TO=(430,110)
    place(d, *BT, "BufferTrack\n[5]", fc=C_PLACE)
    place(d, *TR, "Track\n[0]",       fc=C_PLACE)
    place(d, *OP, "TR_open\n[1]",  tokens=1, fc=C_OPEN)
    place(d, *CL, "TR_close\n[0]", fc=C_CLOSE)
    trans(d, *LA, "landing")
    trans(d, *AO, "addOn\nTrack")
    trans(d, *LK, "lockTR")
    trans(d, *UL, "unlockTR")
    trans(d, *TO, "takeoff")
    # landing: OP<->LA loop, LA->TR
    arrow(d, OP[0], OP[1], LA[0], LA[1])
    arrow(d, LA[0], LA[1], OP[0], OP[1])
    arrow(d, LA[0], LA[1], TR[0], TR[1])
    # addOnTrack: OP<->AO loop, AO->TR
    arrow(d, OP[0], OP[1], AO[0], AO[1])
    arrow(d, AO[0], AO[1], OP[0], OP[1])
    arrow(d, AO[0], AO[1], TR[0], TR[1])
    # lockTR: TR->LK, OP->LK, LK->CL
    arrow(d, TR[0], TR[1], LK[0], LK[1])
    arrow(d, OP[0], OP[1], LK[0], LK[1])
    arrow(d, LK[0], LK[1], CL[0], CL[1])
    # unlockTR: CL->UL, UL->BT, UL->OP
    arrow(d, CL[0], CL[1], UL[0], UL[1])
    arrow(d, UL[0], UL[1], BT[0], BT[1])
    arrow(d, UL[0], UL[1], OP[0], OP[1])
    # takeoff: OP<->TO loop
    arrow(d, OP[0], OP[1], TO[0], TO[1])
    arrow(d, TO[0], TO[1], OP[0], OP[1])
    # Badge "NEW"
    d.add(Rect(AO[0]-22, AO[1]+20, 44, 14, fillColor=C_GREEN,
               strokeColor=C_GREEN, rx=3, ry=3))
    d.add(String(AO[0], AO[1]+23, "NOUVELLE", textAnchor="middle",
                 fontSize=7, fillColor=colors.white, fontName="Helvetica-Bold"))
    return d

# ─── Diagramme : Connexion Garage -> TaxiWay ──────────────────────────────────
def diag_garage_tw():
    d = Drawing(480, 150)
    d.add(Rect(0, 0, 480, 150, fillColor=colors.white, strokeColor=colors.white))
    GA=(60,105); GO=(60,55); BG=(60,155); T=(230,80); TW=(400,105); TWO=(400,55); BTW=(400,155)
    # nodes
    place(d, GA[0],  GA[1],  "Garage",    tokens=1, fc=C_PLACE, r=18)
    place(d, GO[0],  GO[1],  "G_open",    tokens=1, fc=C_OPEN,  r=16)
    place(d, BG[0],  BG[1],  "BufGarage", fc=C_PLACE, r=16)
    trans(d, T[0],   T[1],   "garage_to\n_tw_j", h=38)
    place(d, TW[0],  TW[1],  "TaxiWay_j", fc=C_PLACE, r=18)
    place(d, TWO[0], TWO[1], "TW_open_j", tokens=1, fc=C_OPEN, r=16)
    place(d, BTW[0], BTW[1], "BufTW_j",   fc=C_PLACE, r=16)
    # arcs
    arrow(d, GA[0],  GA[1],  T[0], T[1])         # Garage -> t
    arrow(d, GO[0],  GO[1],  T[0], T[1])          # G_open -> t
    arrow(d, T[0],   T[1],   GO[0], GO[1])         # t -> G_open (loop)
    arrow(d, T[0],   T[1],   TW[0], TW[1])         # t -> TaxiWay_j
    arrow(d, TWO[0], TWO[1], T[0],  T[1])          # TW_open -> t
    arrow(d, T[0],   T[1],   TWO[0], TWO[1])        # t -> TW_open (loop)
    arrow(d, BTW[0], BTW[1], T[0],  T[1])          # BufTW -> t (capacite)
    arrow(d, T[0],   T[1],   BG[0],  BG[1])         # t -> BufGarage
    # labels
    d.add(String(145, 118, "consomme",  textAnchor="middle", fontSize=7, fillColor=colors.grey))
    d.add(String(390, 80,  "produit",   textAnchor="middle", fontSize=7, fillColor=colors.grey))
    return d

# ─── Diagramme : Architecture globale ─────────────────────────────────────────
def diag_architecture():
    d = Drawing(500, 160)
    d.add(Rect(0, 0, 500, 160, fillColor=colors.white, strokeColor=colors.white))
    # Blocs
    def bloc(x, y, w, h, label, fc):
        d.add(Rect(x, y, w, h, fillColor=fc, strokeColor=colors.black,
                   strokeWidth=1.5, rx=6, ry=6))
        d.add(String(x+w/2, y+h/2-5, label, textAnchor="middle",
                     fontSize=10, fontName="Helvetica-Bold", fillColor=colors.black))
    bloc(20,  80, 90, 50, "Garage", C_PLACE)
    bloc(160, 80, 90, 50, "TaxiWay\n× N", C_OPEN)
    bloc(300, 80, 90, 50, "Track\n× N",   C_TRANS)
    bloc(155, 15, 100, 40, "countPlanes", C_COUNT)
    # Flèches principales
    def flat_arrow(x1, y1, x2, y2, label, c=colors.black):
        d.add(Line(x1, y1, x2, y2, strokeColor=c, strokeWidth=2))
        d.add(Polygon([x2, y2, x2-8, y2+4, x2-8, y2-4],
                      fillColor=c, strokeColor=c))
        if label:
            mx, my = (x1+x2)/2, (y1+y2)/2
            d.add(String(mx, my+5, label, textAnchor="middle",
                         fontSize=8, fillColor=c))
    flat_arrow(110, 105, 160, 105, "N trans.", C_GREEN)
    flat_arrow(250, 105, 300, 105, "N² trans.", C_BLUE)
    # Redirection TW->TW
    d.add(Line(205, 130, 205, 145, strokeColor=C_ORANGE, strokeWidth=1.5))
    d.add(Line(205, 145, 240, 145, strokeColor=C_ORANGE, strokeWidth=1.5,
               strokeDashArray=[3,2]))
    d.add(Line(240, 145, 240, 130, strokeColor=C_ORANGE, strokeWidth=1.5))
    d.add(String(222, 150, "N(N-1) redir.", textAnchor="middle",
                 fontSize=7.5, fillColor=C_ORANGE))
    # countPlanes
    d.add(Line(340, 80, 230, 55, strokeColor=C_PURPLE, strokeWidth=1.5,
               strokeDashArray=[3,2]))
    d.add(Line(180, 55, 160, 80, strokeColor=C_PURPLE, strokeWidth=1.5,
               strokeDashArray=[3,2]))
    d.add(String(255, 65, "takeoff/landing/addOnTrack",
                 textAnchor="middle", fontSize=7.5, fillColor=C_PURPLE))
    return d

# ─── Diagramme : countPlanes ──────────────────────────────────────────────────
def diag_count():
    d = Drawing(450, 170)
    d.add(Rect(0, 0, 450, 170, fillColor=colors.white, strokeColor=colors.white))
    CP=(80,  90); BP=(370, 90)
    TO=(225, 145); LA=(225, 60); AO=(225, 20)
    place(d, CP[0], CP[1], "count\nPlanes\n[5]",   tokens=2, fc=C_COUNT)
    place(d, BP[0], BP[1], "Buffer\nCount\nPlanes\n[5]", tokens=2, fc=C_PLACE)
    trans(d, TO[0], TO[1], "takeoff_i")
    trans(d, LA[0], LA[1], "landing_i")
    trans(d, AO[0], AO[1], "addOn\nTrack_i")
    # takeoff: CP-1, BP+1
    arrow(d, CP[0], CP[1], TO[0], TO[1], color=C_RED)
    arrow(d, TO[0], TO[1], BP[0], BP[1], color=C_RED)
    d.add(String(145, 130, "-1", textAnchor="middle", fontSize=9,
                 fillColor=C_RED, fontName="Helvetica-Bold"))
    d.add(String(305, 130, "+1", textAnchor="middle", fontSize=9,
                 fillColor=C_RED, fontName="Helvetica-Bold"))
    # landing: BP-1, CP+1
    arrow(d, BP[0], BP[1], LA[0], LA[1], color=C_BLUE)
    arrow(d, LA[0], LA[1], CP[0], CP[1], color=C_BLUE)
    d.add(String(310, 75, "-1", textAnchor="middle", fontSize=9,
                 fillColor=C_BLUE, fontName="Helvetica-Bold"))
    d.add(String(145, 75, "+1", textAnchor="middle", fontSize=9,
                 fillColor=C_BLUE, fontName="Helvetica-Bold"))
    # addOnTrack: BP-1, CP+1
    arrow(d, BP[0], BP[1], AO[0], AO[1], color=C_GREEN)
    arrow(d, AO[0], AO[1], CP[0], CP[1], color=C_GREEN)
    return d

# ─── Tableau matriciel ────────────────────────────────────────────────────────
def matrix_table(rows, header_row, title=""):
    data = [header_row] + rows
    col_widths = [3.5*cm] + [1.5*cm]*(len(header_row)-1)
    ts = TableStyle([
        ("BACKGROUND", (0,0), (-1,0), C_HEAD),
        ("TEXTCOLOR",  (0,0), (-1,0), colors.white),
        ("FONTNAME",   (0,0), (-1,0), "Helvetica-Bold"),
        ("FONTSIZE",   (0,0), (-1,-1), 8.5),
        ("ALIGN",      (0,0), (-1,-1), "CENTER"),
        ("ALIGN",      (0,1), (0,-1),  "LEFT"),
        ("GRID",       (0,0), (-1,-1), 0.5, C_BORDER),
        ("ROWBACKGROUNDS", (0,1), (-1,-1), [colors.white, colors.HexColor("#F7F9FA")]),
        ("LEFTPADDING",  (0,0), (-1,-1), 5),
        ("RIGHTPADDING", (0,0), (-1,-1), 5),
        ("TOPPADDING",   (0,0), (-1,-1), 3),
        ("BOTTOMPADDING",(0,0), (-1,-1), 3),
    ])
    return Table(data, colWidths=col_widths, style=ts)

def pre_post_tables(places, trans_list, pre_data, post_data, label):
    """Renvoie Pre et Post côte à côte dans une Table externe."""
    def make(mat, title):
        header = [title] + [f"T{i}\n{t}" for i, t in enumerate(trans_list)]
        rows   = [[p] + [str(v) for v in row]
                  for p, row in zip(places, mat)]
        return matrix_table(rows, header)
    pre_t  = make(pre_data,  "Pre")
    post_t = make(post_data, "Post")
    outer = Table([[pre_t, post_t]], colWidths=None,
                  style=TableStyle([("VALIGN",(0,0),(-1,-1),"TOP"),
                                    ("LEFTPADDING",(0,0),(-1,-1),5)]))
    return outer

# ─── Code ─────────────────────────────────────────────────────────────────────
def code(text):
    return Preformatted(text, sCode)

def code_block(text):
    return KeepTogether([
        Spacer(1, 0.2*cm),
        code(text),
        Spacer(1, 0.2*cm),
    ])

# ─── Construction du document ─────────────────────────────────────────────────
def build():
    out_path = os.path.join(os.path.dirname(__file__), "documentation_petri.pdf")
    doc = SimpleDocTemplate(
        out_path, pagesize=A4,
        leftMargin=2.2*cm, rightMargin=2.2*cm,
        topMargin=2.2*cm, bottomMargin=2.2*cm,
        title="Construction du Réseau de Petri – CY-SKY",
        author="CY-Sky",
    )
    story = []
    P = Paragraph  # alias

    def hr():
        return HRFlowable(width="100%", thickness=0.5,
                          color=C_BORDER, spaceAfter=4)

    # ── PAGE DE TITRE ─────────────────────────────────────────────────────────
    story += [
        Spacer(1, 2*cm),
        P("Construction du Réseau de Petri", sTitle),
        P("CY-SKY — Modélisation de la Gestion du Trafic Aérien à Orly", sSubtitle),
        Spacer(1, 0.5*cm),
        hr(),
        Spacer(1, 4*cm),
        P("Ce document décrit pas à pas la construction formelle du réseau de Petri "
          "utilisé dans le projet CY-SKY pour simuler la gestion des avions "
          "sur l'aéroport d'Orly : modules de base, opérations de composition, "
          "connexions inter-modules, compteur global et vérification des invariants.",
          sBody),
        Spacer(1, 2*cm),
        PageBreak(),
    ]

    # ── 1. INTRODUCTION ───────────────────────────────────────────────────────
    story += [
        P("1. Introduction", sH1), hr(),
        P("1.1 Contexte", sH2),
        P("Le projet CY-SKY modélise formellement la circulation des avions dans "
          "l'aéroport d'Orly. Le système comprend un garage, N taxiways et N pistes "
          "de décollage/atterrissage (N = 3). Toutes les ressources sont partagées "
          "et requièrent une gestion rigoureuse de la concurrence.", sBody),

        P("1.2 Définition formelle d'un réseau de Petri", sH2),
        P("Un réseau de Petri est un 5-uplet <b>(P, T, Pre, Post, M₀)</b> où :", sBody),
        P("• <b>P</b> : ensemble fini de <b>places</b> (états, ressources) — représentées par des cercles", sBullet),
        P("• <b>T</b> : ensemble fini de <b>transitions</b> (actions) — représentées par des rectangles", sBullet),
        P("• <b>Pre ∈ ℕ<sup>|P|×|T|</sup></b> : matrice de pré-incidence (consommation de jetons)", sBullet),
        P("• <b>Post ∈ ℕ<sup>|P|×|T|</sup></b> : matrice de post-incidence (production de jetons)", sBullet),
        P("• <b>M₀ ∈ ℕ<sup>|P|</sup></b> : marquage initial", sBullet),
        Spacer(1, 0.3*cm),
        P("<b>Franchissement :</b> Une transition t est franchissable si M(p) ≥ Pre(p,t) ∀p. "
          "Le nouveau marquage est M'(p) = M(p) − Pre(p,t) + Post(p,t).", sInfo),

        Spacer(1, 0.4*cm),
        P("1.3 Paramètres du système", sH2),
        matrix_table(
            [["N", "Nombre de taxiways et de pistes", "3", "val N = 3"],
             ["nAvions", "Avions initiaux dans le système", "5", "val nAvions = 5"],
             ["nMaxSystem", "Capacité maximale du système", "10", "val nMaxSystem = 10"]],
            ["Paramètre", "Description", "Valeur", "Code Scala"],
        ),
        Spacer(1, 0.4*cm),

        P("1.4 Architecture générale", sH2),
        diag_architecture(),
        P("Figure 1 — Flux des avions : Garage → TaxiWay → Track, avec redirections et compteur global.", sCap),
        PageBreak(),
    ]

    # ── 2. VERROUS ────────────────────────────────────────────────────────────
    story += [
        P("2. Convention des Verrous (Open / Close)", sH1), hr(),
        P("Chaque zone dispose d'un mécanisme d'<b>exclusion mutuelle</b> implémenté "
          "par deux places complémentaires : <b>open</b> (zone ouverte) et "
          "<b>close</b> (zone fermée).", sBody),

        P("Invariant de verrou : pour chaque module k, open_k + close_k = 1 ∀t.", sInfo),

        Spacer(1, 0.3*cm),
        diag_lock(),
        P("Figure 2 — Cycle du verrou : lock consomme open et produit close ; unlock fait l'inverse.", sCap),

        Spacer(1, 0.3*cm),
        P("⚠ Erreur courante et correction :", sH3),
        P("Si <b>lock ne consomme pas open</b>, les jetons s'accumulent dans open à chaque cycle "
          "lock/unlock (ex. open = 45 après 45 cycles). La correction est :", sWarn),
        code_block("// AVANT (bugué) :\npre(G_open, lockGarage) = 0  // lock ignore open !\n\n"
                   "// APRÈS (corrigé) :\npre(G_open, lockGarage) = 1  // lock doit consommer open"),
        matrix_table(
            [["open",  "1", "0"],
             ["close", "0", "1"]],
            ["Place", "Pre(lock)", "Post(lock)"],
        ),
        P("Table 1 — Le verrou lock doit avoir Pre(open) = 1 pour respecter l'invariant.", sCap),
        PageBreak(),
    ]

    # ── 3. MODULES DE BASE ────────────────────────────────────────────────────
    story += [P("3. Modules de Base", sH1), hr()]

    # ─ 3.1 Garage ─
    story += [
        P("3.1 Module Garage", sH2),
        P("Héberge les avions en attente de départ. Il gère le stock d'avions "
          "(<b>Garage</b>) et les places libres (<b>BufferGarage</b>).", sBody),
        matrix_table(
            [["BufferGarage", "Places libres au garage", "nMax − nAvions = 5"],
             ["Garage",       "Avions présents au garage", "nAvions = 5"],
             ["G_open",       "Garage ouvert (verrou)", "1"],
             ["G_close",      "Garage fermé  (verrou)", "0"]],
            ["Place", "Signification", "M₀"],
        ),
        Spacer(1, 0.3*cm),
        diag_garage(),
        P("Figure 3 — Module Garage : 4 places, 2 transitions.", sCap),
        Spacer(1, 0.3*cm),
        pre_post_tables(
            ["BufferGarage", "Garage", "G_open", "G_close"],
            ["lockG", "unlockG"],
            [[0,0],[1,0],[1,0],[0,1]],
            [[0,1],[0,0],[0,1],[1,0]],
            "Garage"
        ),
        P("Table 2 — Matrices Pre et Post du module Garage (G_open est consommé par lockGarage).", sCap),
        Spacer(1, 0.3*cm),
    ]

    # ─ 3.2 TaxiWay ─
    story += [
        P("3.2 Module TaxiWay", sH2),
        P("Voie de circulation entre le garage et les pistes. Ce module est "
          "<b>répliqué N = 3 fois</b> avec suffixe _1, _2, _3.", sBody),
        matrix_table(
            [["BufferTaxiWay","Places libres sur le taxiway","nMax − nAvions = 5"],
             ["TaxiWay",      "Avions présents sur le taxiway","0"],
             ["TW_open",      "Taxiway ouvert (verrou)","1"],
             ["TW_close",     "Taxiway fermé  (verrou)","0"]],
            ["Place", "Signification", "M₀"],
        ),
        Spacer(1, 0.3*cm),
        diag_taxiway(),
        P("Figure 4 — Module TaxiWay : 4 places, 3 transitions.", sCap),
        Spacer(1, 0.3*cm),
        pre_post_tables(
            ["BufferTaxiWay","TaxiWay","TW_open","TW_close"],
            ["addTW","lockTW","unlockTW"],
            [[0,0,0],[0,1,0],[1,1,0],[0,0,1]],
            [[0,0,1],[1,0,0],[1,0,1],[0,1,0]],
            "TaxiWay"
        ),
        P("Table 3 — Matrices Pre/Post du module TaxiWay (TW_open est consommé par lockTaxiWay).", sCap),
        PageBreak(),
    ]

    # ─ 3.3 Track ─
    story += [
        P("3.3 Module Track (Piste)", sH2),
        P("Piste de décollage/atterrissage. Répliqué N = 3 fois. "
          "Comporte <b>5 transitions</b> dont <b>addOnTrack</b> (transition ajoutée "
          "par rapport au schéma initial) pour modéliser les arrivées directes.", sBody),
        matrix_table(
            [["BufferTrack","Places libres sur la piste","nMax − nAvions = 5"],
             ["Track",      "Avions présents sur la piste","0"],
             ["TR_open",    "Piste ouverte (verrou)","1"],
             ["TR_close",   "Piste fermée  (verrou)","0"]],
            ["Place", "Signification", "M₀"],
        ),
        Spacer(1, 0.3*cm),
        diag_track(),
        P("Figure 5 — Module Track : 4 places, 5 transitions. addOnTrack (en vert) est la transition ajoutée.", sCap),
        Spacer(1, 0.3*cm),
        pre_post_tables(
            ["BufferTrack","Track","TR_open","TR_close"],
            ["landing","lockTR","unlockTR","takeoff","addOn"],
            [[0,0,0,0,0],[0,1,0,0,0],[1,1,0,1,1],[0,0,1,0,0]],
            [[0,0,1,0,0],[1,0,0,0,1],[1,0,1,1,1],[0,1,0,0,0]],
            "Track"
        ),
        P("Table 4 — Matrices Pre/Post du module Track (TR_open est consommé par lockTrack).", sCap),
        PageBreak(),
    ]

    # ── 4. OPÉRATIONS DE COMPOSITION ─────────────────────────────────────────
    story += [
        P("4. Opérations de Composition", sH1), hr(),

        P("4.1 blockDiag — Composition bloc-diagonale", sH2),
        P("Assemble deux modules A et B sans les connecter. Les matrices résultantes "
          "sont bloc-diagonales : les deux sous-réseaux restent indépendants.", sBody),
        code_block(
"// Pre(A⊕B) = | Pre_A   0   |      Post(A⊕B) = | Post_A   0   |\n"
"//            |   0   Pre_B |                   |    0   Post_B|\n\n"
"def blockDiag(a: PetriModule, b: PetriModule): PetriModule = {\n"
"  val (tA, tB) = (a.transitions.length, b.transitions.length)\n"
"  PetriModule(\n"
"    places      = a.places ++ b.places,\n"
"    transitions = a.transitions ++ b.transitions,\n"
"    pre  = a.pre.map(_ ++ Vector.fill(tB)(0)) ++\n"
"           b.pre.map(Vector.fill(tA)(0) ++ _),\n"
"    post = a.post.map(_ ++ Vector.fill(tB)(0)) ++\n"
"           b.post.map(Vector.fill(tA)(0) ++ _),\n"
"    marking = a.marking ++ b.marking\n"
"  )\n"
"}"),

        P("4.2 replicateModule — Réplication d'un module", sH2),
        P("Crée N copies d'un module de base avec noms indexés "
          "(_1, _2, ..., _N) assemblées par bloc-diagonal.", sBody),
        code_block(
"def replicateModule(base: PetriModule, n: Int): PetriModule =\n"
"  (1 to n).map { i =>\n"
"    base.copy(\n"
"      places      = base.places.map(p => s\"${p}_$i\"),\n"
"      transitions = base.transitions.map(t => s\"${t}_$i\")\n"
"    )\n"
"  }.reduce(blockDiag)"),

        P("4.3 addLinkTransition — Transition de lien", sH2),
        P("Ajoute une transition qui consomme dans une place source "
          "et produit dans une place destination. C'est l'opération principale "
          "pour créer des connexions <b>inter-modules</b>.", sBody),
        code_block(
"// Pre(p, t_new) = 1 si p == fromPlace, 0 sinon\n"
"// Post(p, t_new) = 1 si p == toPlace,   0 sinon\n"
"def addLinkTransition(m: PetriModule, name: String,\n"
"                      fromPlace: String, toPlace: String): PetriModule"),

        P("4.4 addArc — Ajout d'un arc", sH2),
        P("Modifie le poids d'un arc existant (ou en crée un) entre une place "
          "et une transition :", sBody),
        code_block(
"// Pre(p, t) += preCost    Post(p, t) += postGain\n"
"def addArc(m: PetriModule, place: String, transition: String,\n"
"           preCost: Int, postGain: Int): PetriModule"),
        PageBreak(),
    ]

    # ── 5. CONSTRUCTION GLOBALE ───────────────────────────────────────────────
    story += [
        P("5. Construction du Réseau Global", sH1), hr(),

        P("5.1 Étape 1 — Réplication et assemblage diagonal", sH2),
        code_block(
"val allTaxiWays = replicateModule(taxiWayBase, N)  // 3 copies indexées\n"
"val allTracks   = replicateModule(trackBase,   N)  // 3 copies indexées\n"
"val base = blockDiag(garage, blockDiag(allTaxiWays, allTracks))"),
        matrix_table(
            [["Garage",        "4",  "2",  "—"],
             ["TaxiWay × 3",   "12", "9",  "—"],
             ["Track × 3",     "12", "15", "—"],
             ["Total étape 1", "28", "26", "0 connexion inter-module"]],
            ["Module", "Places", "Transitions", "Note"],
        ),
        Spacer(1, 0.3*cm),

        P("5.2 Étape 2 — Connexions Garage → TaxiWay_j (N = 3 transitions)", sH2),
        diag_garage_tw(),
        P("Figure 6 — Transition garage_to_tw_j : vérifications de capacité et d'état ouvert.", sCap),
        code_block(
"val withGarageTw = (1 to N).foldLeft(base) { (sys, j) =>\n"
"  val s1 = addLinkTransition(sys, s\"garage_to_tw$j\", \"Garage\", s\"TaxiWay_$j\")\n"
"  val s2 = addArc(s1, s\"BufferTaxiWay_$j\", s\"garage_to_tw$j\", 1, 0)  // cap. TW\n"
"  val s3 = addArc(s2, \"BufferGarage\",       s\"garage_to_tw$j\", 0, 1)  // lib. garage\n"
"  val s4 = addArc(s3, \"G_open\",             s\"garage_to_tw$j\", 1, 1)  // garage ouvert?\n"
"  addArc(s4,          s\"TW_open_$j\",        s\"garage_to_tw$j\", 1, 1)  // TW ouvert?\n"
"}"),

        P("5.3 Étape 3 — Connexions TaxiWay_i → Track_j (N² = 9 transitions)", sH2),
        P("Pour chaque paire (i, j) ∈ {1,2,3}², la transition tw_i_to_tr_j :", sBody),
        matrix_table(
            [["pre(TaxiWay_i)",  "1", "Un avion quitte le taxiway i"],
             ["post(Track_j)",   "1", "Un avion arrive sur la piste j"],
             ["pre(BufferTrack_j)", "1", "Vérifie la capacité de la piste j"],
             ["post(BufferTaxiWay_i)", "1", "Libère une place sur le taxiway i"],
             ["pre/post(TR_open_j)",   "1/1", "Boucle : piste j doit être ouverte"]],
            ["Arc", "Poids", "Signification"],
        ),
        Spacer(1, 0.3*cm),

        P("5.4 Étape 4 — Redirections TaxiWay_i → TaxiWay_j (N(N−1) = 6 transitions)", sH2),
        P("Permettent de réacheminer un avion d'un taxiway à un autre (pour i ≠ j). "
          "Chaque transition consomme TaxiWay_i, produit TaxiWay_j, "
          "et boucle sur TW_open_j.", sBody),
        code_block(
"val withRedirects = (for (i <- 1 to N; j <- 1 to N; if i != j) yield (i, j))\n"
"  .foldLeft(withTwTr) { case (sys, (i, j)) =>\n"
"    val s1 = addLinkTransition(sys, s\"redirect_tw${i}_to_tw${j}\",\n"
"                               s\"TaxiWay_$i\", s\"TaxiWay_$j\")\n"
"    addArc(s1, s\"TW_open_$j\", s\"redirect_tw${i}_to_tw${j}\", 1, 1)\n"
"  }"),
        Spacer(1, 0.3*cm),

        P("5.5 Étape 5 — Compteur global d'avions", sH2),
        P("Deux places globales garantissent que le nombre d'avions actifs "
          "reste dans [0, nMaxSystem].", sBody),
        P("Invariant : countPlanes + BufferCountPlanes = nMaxSystem = 10 ∀t", sInfo),
        diag_count(),
        P("Figure 7 — Les trois transitions qui interagissent avec le compteur global.", sCap),
        code_block(
"val withCount = addPlace(\n"
"  addPlace(withRedirects, \"countPlanes\", nAvions),\n"
"  \"BufferCountPlanes\", nMaxSystem - nAvions)\n\n"
"val system = (1 to N).foldLeft(withCount) { (sys, i) =>\n"
"  val s1 = addArc(sys, \"countPlanes\",       s\"takeoff_$i\",    1, 0)\n"
"  val s2 = addArc(s1,  \"BufferCountPlanes\", s\"takeoff_$i\",    0, 1)\n"
"  val s3 = addArc(s2,  \"BufferCountPlanes\", s\"landing_$i\",    1, 0)\n"
"  val s4 = addArc(s3,  \"countPlanes\",       s\"landing_$i\",    0, 1)\n"
"  val s5 = addArc(s4,  \"BufferCountPlanes\", s\"addOnTrack_$i\", 1, 0)\n"
"  addArc(s5,           \"countPlanes\",       s\"addOnTrack_$i\", 0, 1)\n"
"}"),
        PageBreak(),
    ]

    # ── 6. SYNTHÈSE ───────────────────────────────────────────────────────────
    story += [
        P("6. Synthèse du Réseau Complet (N = 3)", sH1), hr(),
        matrix_table(
            [["Modules de base (étape 1)",             "28", "26"],
             ["Connexions Garage→TW (étape 2)",         "0",   "3"],
             ["Connexions TW→Track  (étape 3)",         "0",   "9"],
             ["Redirections TW→TW   (étape 4)",         "0",   "6"],
             ["Compteur global      (étape 5)",         "2",   "0"],
             ["TOTAL",                                 "30",  "44"]],
            ["Origine", "Places", "Transitions"],
        ),
        Spacer(1, 0.3*cm),
        P("Marquage initial M₀ :", sH3),
        P("[5, 5, 1, 0,  5,0,1,0,  5,0,1,0,  5,0,1,0,  "
          "5,0,1,0,  5,0,1,0,  5,0,1,0,  5, 5]", sCenter),
        P(" Garage(4)   TW₁(4)   TW₂(4)   TW₃(4)   TR₁(4)   TR₂(4)   TR₃(4)  cnt buf", sCap),
        PageBreak(),
    ]

    # ── 7. VÉRIFICATION ───────────────────────────────────────────────────────
    story += [
        P("7. Vérification Formelle et Test", sH1), hr(),

        P("7.1 Invariants structurels", sH2),
        P("<b>I1 — Aucun jeton négatif :</b>  ∀p ∈ P, ∀t : M_t(p) ≥ 0", sBody),
        P("<b>I2 — Conservation du compteur :</b>  countPlanes + BufferCountPlanes = 10", sBody),
        P("<b>I3 — Exclusion mutuelle des verrous :</b>  open_k + close_k = 1 "
          "pour chaque module k (Garage, TW₁, TW₂, TW₃, TR₁, TR₂, TR₃)", sBody),
        Spacer(1, 0.3*cm),

        P("7.2 Fonction verify", sH2),
        code_block(
"def verify(m: PetriModule): (Boolean, List[String]) = {\n"
"  val errors = scala.collection.mutable.ListBuffer[String]()\n"
"  // I1 : pas de jetons negatifs\n"
"  m.marking.zipWithIndex.foreach { case (tok, i) =>\n"
"    if (tok < 0) errors += s\"Negatif : ${m.places(i)} = $tok\"\n"
"  }\n"
"  // I2 : conservation countPlanes\n"
"  val total = m.marking(m.places.indexOf(\"countPlanes\")) +\n"
"              m.marking(m.places.indexOf(\"BufferCountPlanes\"))\n"
"  if (total != nMaxSystem) errors += s\"Conservation : $total != $nMaxSystem\"\n"
"  // I3 : exclusion mutuelle des verrous\n"
"  def checkLock(o: String, c: String): Unit = {\n"
"    val s = m.marking(m.places.indexOf(o)) + m.marking(m.places.indexOf(c))\n"
"    if (s != 1) errors += s\"Verrou : $o + $c = $s\"\n"
"  }\n"
"  checkLock(\"G_open\", \"G_close\")\n"
"  (1 to N).foreach { i =>\n"
"    checkLock(s\"TW_open_$i\", s\"TW_close_$i\")\n"
"    checkLock(s\"TR_open_$i\", s\"TR_close_$i\")\n"
"  }\n"
"  (errors.isEmpty, errors.toList)\n"
"}"),

        P("7.3 Test aléatoire (1000 transitions)", sH2),
        code_block(
"def randomTest(initial: PetriModule, steps: Int = 1000): Unit = {\n"
"  val rng = new scala.util.Random(42) // graine fixe = reproductible\n"
"  var state = initial\n"
"  var fired = 0; var deadlocks = 0; var errors = 0\n"
"  for (step <- 1 to steps) {\n"
"    val enabled = enabledTransitions(state) // T franchissables\n"
"    if (enabled.isEmpty) {\n"
"      deadlocks += 1; state = initial      // deadlock -> reinit\n"
"    } else {\n"
"      val t = enabled(rng.nextInt(enabled.length))\n"
"      state = fireTransition(state, t)      // M' = M - Pre[t] + Post[t]\n"
"      fired += 1\n"
"      val (ok, errs) = verify(state)\n"
"      if (!ok) { errors += 1; errs.foreach(println) }\n"
"    }\n"
"  }\n"
"  if (errors == 0 && deadlocks == 0)\n"
"    println(s\"=> Test REUSSI : aucune erreur sur $steps transitions\")\n"
"}"),

        Spacer(1, 0.3*cm),
        P("Résultat attendu :", sH3),
        code_block(
"=== TEST ALEATOIRE (1000 transitions) ===\n"
"  Transitions franchies    : 1000\n"
"  Deadlocks rencontres     : 0\n"
"  Violations d'invariants  : 0\n"
"  => Test REUSSI : aucune erreur sur 1000 transitions"),
        PageBreak(),
    ]

    # ── 8. CONCLUSION ─────────────────────────────────────────────────────────
    story += [
        P("8. Conclusion", sH1), hr(),
        P("Le réseau de Petri CY-SKY modélise formellement la gestion du trafic "
          "aérien à Orly selon une architecture en trois couches :", sBody),
        P("1. <b>Modules de base</b> (Garage, TaxiWay, Track) : chacun encapsule "
          "un mécanisme de verrou open/close garantissant l'exclusion mutuelle.", sBullet),
        P("2. <b>Composition modulaire</b> : les modules sont répliqués N fois "
          "et assemblés par bloc-diagonal, puis interconnectés par des transitions "
          "de lien ciblées.", sBullet),
        P("3. <b>Gestion globale</b> : un compteur partagé (countPlanes / "
          "BufferCountPlanes) garantit que le nombre total d'avions dans le "
          "système reste borné par nMaxSystem = 10.", sBullet),
        Spacer(1, 0.5*cm),
        matrix_table(
            [["Absence de jetons négatifs",    "✓ (vérifiée par verify)"],
             ["Conservation du parc d'avions", "✓ (invariant de Petri)"],
             ["Exclusion mutuelle des pistes",  "✓ (invariant de verrou)"],
             ["Absence de deadlock (test)",     "✓ (1000 transitions)"]],
            ["Propriété", "Statut"],
        ),
        Spacer(1, 0.5*cm),
        P("La structure modulaire facilite le passage à l'échelle : modifier N "
          "suffit pour adapter le réseau à un aéroport avec plus ou moins de "
          "taxiways et de pistes.", sBody),
    ]

    doc.build(story)
    print(f"PDF généré : {out_path}")
    return out_path

if __name__ == "__main__":
    build()
