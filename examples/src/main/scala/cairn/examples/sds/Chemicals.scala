package cairn.examples.sds

/** Thin chemicals corpus fixture for SDS EU-CLP outlines.
  *
  * Host-side only: Cairn's `sds.cairn` objects cover substances / mixtures /
  * products / phrases; section bodies are not yet first-class language
  * constructors. This pack advances acetone toward a fuller REACH Annex II
  * (EU-CLP) 16-section outline with honest placeholder/content where known,
  * validated by [[SectionNumbering]]. Secondary chemicals stay sparse.
  *
  * Report projection of these maps is [[SectionReport]] (host GrammarSpec +
  * RoundTrip) — still not `sds.cairn` constructors. Not Studio: no report UI,
  * no persisted phrase-corpus editor, no section authoring surface — see
  * STATUS-2 / docs/exemplars remaining gaps.
  */
object Chemicals:
  /** One populated EU-CLP section body (EN text fields; demo data only). */
  final case class SectionBody(number: Int, fields: Map[String, String]):
    def title: String =
      SectionNumbering.byNumber.getOrElse(number, s"INVALID-$number")
    def outlineEntry: SectionNumbering.OutlineEntry =
      SectionNumbering.OutlineEntry(number, title)

  /** A chemical document as a (possibly sparse) map of section bodies. */
  final case class ChemicalDoc(
      name: String,
      cas: String,
      sections: Map[Int, SectionBody]
  ):
    def outline: List[SectionNumbering.OutlineEntry] =
      sections.keys.toList.sorted.map(n => sections(n).outlineEntry)

    def populatedNumbers: List[Int] = sections.keys.toList.sorted

    def validateOutline: Either[List[SectionNumbering.OutlineError], List[SectionNumbering.SectionDef]] =
      SectionNumbering.validateOutline(outline)

  /** Acetone: fuller EU-CLP outline (all 16 sections). Content is
    * demonstration / literature-shaped placeholder — not a regulatory filing.
    * Section 16 flags the demo nature explicitly.
    */
  object Acetone:
    val pure: ChemicalDoc = ChemicalDoc(
      name = "Acetone",
      cas = "67-64-1",
      sections = Map(
        1 -> SectionBody(1, Map(
          "productName" -> "Acetone",
          "synonyms" -> "2-Propanone; propan-2-one",
          "recommendedUse" -> "Laboratory reagent; solvent for coatings, adhesives, and cleaning.",
          "usesAdvisedAgainst" -> "Food, drug, pesticide, or biocidal product use.",
          "supplierName" -> "Cairn Chemicals Demo Ltd. (fictional, for demonstration only)",
          "emergencyPhone" -> "+32 000 000 000 (demo number)")),
        2 -> SectionBody(2, Map(
          "classificationSummary" ->
            "Flammable liquid Category 2; Serious eye damage/eye irritation Category 2; STOT SE Category 3 (CNS) — Regulation (EC) No 1272/2008",
          "hazardsNotOtherwiseClassified" -> "Repeated exposure may cause skin dryness or cracking.",
          "hazardPhrases" -> "H225; H319; H336",
          "signalWord" -> "Danger",
          "pictograms" -> "GHS02; GHS07")),
        3 -> SectionBody(3, Map(
          "componentName" -> "Acetone",
          "cas" -> "67-64-1",
          "ec" -> "200-662-2",
          "concentration" -> ">= 99.5 %")),
        4 -> SectionBody(4, Map(
          "generalAdvice" -> "If symptoms persist, call a physician.",
          "inhalation" -> "Remove to fresh air. If breathing is difficult, give oxygen.",
          "skinContact" -> "Wash off with soap and plenty of water. Remove contaminated clothing.",
          "eyeContact" -> "Rinse cautiously with water for several minutes. Remove contact lenses if present.",
          "ingestion" -> "Rinse mouth. Do NOT induce vomiting.")),
        5 -> SectionBody(5, Map(
          "extinguishingMedia" -> "Water spray, alcohol-resistant foam, dry chemical, or carbon dioxide.",
          "unsuitableExtinguishingMedia" -> "Water may be ineffective as the primary extinguishing agent.",
          "specialHazards" -> "Vapours may form explosive mixtures with air; containers may rupture when heated.",
          "firefighterProtection" -> "Wear SCBA and full protective gear.")),
        6 -> SectionBody(6, Map(
          "personalPrecautions" -> "Remove ignition sources. Ventilate. Wear appropriate PPE.",
          "environmentalPrecautions" -> "Prevent entry into drains and waterways.",
          "cleanupMethods" -> "Contain spill; soak up with inert absorbent; dispose as hazardous waste.")),
        7 -> SectionBody(7, Map(
          "handling" -> "Avoid contact with skin and eyes. Use only in well-ventilated areas. Keep away from heat and open flames.",
          "storage" -> "Store cool, well-ventilated, away from ignition sources. Keep container tightly closed.",
          "storageIncompatibilities" -> "Strong oxidizers, strong reducers, strong bases, peroxides, halogenated compounds, alkali metals, amines.")),
        8 -> SectionBody(8, Map(
          "occupationalExposureLimit" -> "EU indicative OELV 500 ppm (1210 mg/m3), 8-hour TWA; ACGIH TWA 250 ppm / STEL 500 ppm.",
          "engineeringControls" -> "Adequate ventilation; explosion-proof electrical equipment; eyewash nearby.",
          "eyeProtection" -> "Chemical safety goggles (e.g. EN166).",
          "skinProtection" -> "Protective gloves and clothing.",
          "respiratoryProtection" -> "Organic vapor respirator (type AX) if exposure limits exceeded.")),
        9 -> SectionBody(9, Map(
          "appearance" -> "Colourless liquid.",
          "odor" -> "Characteristic, sweetish (mint-like).",
          "molecularWeight" -> "58.08 g/mol",
          "meltingPoint" -> "-94.7 degC",
          "boilingPoint" -> "56.05 degC at 1013 hPa",
          "flashPoint" -> "-20 degC (closed cup)",
          "density" -> "0.79 g/cm3 at 20 degC",
          "solubility" -> "Miscible with water in all proportions.",
          "explosiveLimits" -> "Lower 2.5% / Upper 13% (v/v in air)")),
        10 -> SectionBody(10, Map(
          "stability" -> "Stable under recommended storage conditions.",
          "conditionsToAvoid" -> "Heat, flames, and sparks.",
          "incompatibleMaterials" -> "Strong oxidizing agents, strong bases, peroxides, halogenated compounds, alkali metals, amines.",
          "hazardousDecomposition" -> "CO, CO2, formaldehyde, methanol under fire conditions.")),
        11 -> SectionBody(11, Map(
          "LD50Oral" -> "5800 mg/kg (rat, oral)",
          "irritation" -> "Causes serious eye irritation. Prolonged skin contact may cause dryness.",
          "inhalationEffects" -> "Vapours may cause drowsiness and dizziness.",
          "carcinogenicity" -> "Not listed as a carcinogen by IARC, NTP, ACGIH, or OSHA.")),
        12 -> SectionBody(12, Map(
          "ecotoxicity" -> "Fish LC50 ~5540 mg/L (96h); daphnia EC50 ~8800 mg/L (48h).",
          "persistence" -> "Readily biodegradable; persistence unlikely.",
          "bioaccumulation" -> "Low (log Pow = -0.24).",
          "mobility" -> "Likely mobile due to volatility.")),
        13 -> SectionBody(13, Map(
          "disposalMethods" -> "Dispose per local/regional/national regulations. Do not discharge to drains.",
          "wasteClassification" -> "Consult local regulations; US RCRA U002 is often cited for acetone waste.")),
        14 -> SectionBody(14, Map(
          "unNumber" -> "UN1090",
          "properShippingName" -> "ACETONE",
          "transportHazardClass" -> "Class 3 (Flammable liquid)",
          "packingGroup" -> "II")),
        15 -> SectionBody(15, Map(
          "regulatoryInfo" -> "Classified and labelled per Regulation (EC) No 1272/2008 (CLP).",
          "reachStatus" -> "Subject to REACH Annex XVII entry 75 use restriction (demo note).",
          "usInventory" -> "Listed on US TSCA Inventory (active).")),
        16 -> SectionBody(16, Map(
          "revisionDate" -> "2026-07-22",
          "otherInformation" ->
            "Demo/example data for the Cairn SDS chemicals corpus — not regulatory or safety advice for real use."))))

    def outline: List[SectionNumbering.OutlineEntry] = pure.outline

  /** Ethanol: sparse secondary substance (identification + hazards only). */
  object Ethanol:
    val pure: ChemicalDoc = ChemicalDoc(
      name = "Ethanol",
      cas = "64-17-5",
      sections = Map(
        1 -> SectionBody(1, Map("productName" -> "Ethanol")),
        2 -> SectionBody(2, Map(
          "hazardPhrases" -> "H225; H319",
          "signalWord" -> "Danger"))))

  /** Tutorial spine still speaks only to hazards + composition; kept here so
    * the sparse vs fuller acetone outlines stay explicit.
    */
  val acetoneTutorialSparse: List[SectionNumbering.OutlineEntry] =
    SectionNumbering.acetoneSparseOutline
