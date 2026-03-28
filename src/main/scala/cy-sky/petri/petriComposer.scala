package cysky.petri

case class PetriModule(
  places:      Vector[String],
  transitions: Vector[String],
  pre:         Vector[Vector[Int]],
  post:        Vector[Vector[Int]],
  marking:     Vector[Int]
)

object PetriComposer {

  def blockDiag(a: PetriModule, b: PetriModule): PetriModule = {
    val tA = a.transitions.length
    val tB = b.transitions.length
    PetriModule(
      places      = a.places ++ b.places,
      transitions = a.transitions ++ b.transitions,
      pre         = a.pre.map(_ ++ Vector.fill(tB)(0)) ++
                    b.pre.map(Vector.fill(tA)(0) ++ _),
      post        = a.post.map(_ ++ Vector.fill(tB)(0)) ++
                    b.post.map(Vector.fill(tA)(0) ++ _),
      marking     = a.marking ++ b.marking
    )
  }

  def mergePlaces(m: PetriModule, nameA: String, nameB: String): PetriModule = {
    val iA = m.places.indexOf(nameA)
    val iB = m.places.indexOf(nameB)
    require(iA >= 0 && iB >= 0, s"$nameA / $nameB introuvables")
    val mergedPre  = m.pre(iA).zip(m.pre(iB)).map { case (a, b) => a + b }
    val mergedPost = m.post(iA).zip(m.post(iB)).map { case (a, b) => a + b }
    val mergedMark = m.marking(iA) + m.marking(iB)
    m.copy(
      places  = m.places.patch(iB, Nil, 1),
      pre     = m.pre.updated(iA, mergedPre).patch(iB, Nil, 1),
      post    = m.post.updated(iA, mergedPost).patch(iB, Nil, 1),
      marking = m.marking.updated(iA, mergedMark).patch(iB, Nil, 1)
    )
  }

  // --- Ajouter une transition de lien entre deux places existantes ---
  def addLinkTransition(
    m:         PetriModule,
    name:      String,
    fromPlace: String,
    toPlace:   String
  ): PetriModule = {
    val iFrom = m.places.indexOf(fromPlace)
    val iTo   = m.places.indexOf(toPlace)
    require(iFrom >= 0 && iTo >= 0, s"$fromPlace / $toPlace introuvables")

    // Nouvelle colonne : consomme 1 dans fromPlace, produit 1 dans toPlace
    val newPreCol  = Vector.tabulate(m.places.length)(i => if (i == iFrom) 1 else 0)
    val newPostCol = Vector.tabulate(m.places.length)(i => if (i == iTo)   1 else 0)

    m.copy(
      transitions = m.transitions :+ name,
      pre         = m.pre.zip(newPreCol).map   { case (row, v) => row :+ v },
      post        = m.post.zip(newPostCol).map  { case (row, v) => row :+ v }
    )
  }

  // --- Ajouter une place (ex: countPlanes global) ---
  def addPlace(m: PetriModule, name: String, tokens: Int): PetriModule = {
    m.copy(
      places  = m.places :+ name,
      pre     = m.pre :+ Vector.fill(m.transitions.length)(0),
      post    = m.post :+ Vector.fill(m.transitions.length)(0),
      marking = m.marking :+ tokens
    )
  }

  // --- Connecter une place existante à une transition existante ---
  def addArc(
    m:          PetriModule,
    place:      String,
    transition: String,
    preCost:    Int,
    postGain:   Int
  ): PetriModule = {
    val iP = m.places.indexOf(place)
    val jT = m.transitions.indexOf(transition)
    require(iP >= 0 && jT >= 0)
    m.copy(
      pre  = m.pre.updated(iP, m.pre(iP).updated(jT, m.pre(iP)(jT) + preCost)),
      post = m.post.updated(iP, m.post(iP).updated(jT, m.post(iP)(jT) + postGain))
    )
  }

  def replicateModule(base: PetriModule, n: Int): PetriModule = {
    (1 to n).map { i =>
      base.copy(
        places      = base.places.map(p => s"${p}_$i"),
        transitions = base.transitions.map(t => s"${t}_$i")
      )
    }.reduce(blockDiag)
  }
}