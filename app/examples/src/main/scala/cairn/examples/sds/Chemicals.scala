package cairn.examples.sds
import cairn.runtime.EffectContexts

import cairn.kernel.*
import cairn.core.*
import cairn.runtime.PackLoader

/** Chemicals corpus — **disk `.cairn` via [[ChemicalSource]] is SoT**.
  * Host [[ChemicalDoc]] maps are emit fixtures ([[EmitChemicalCairn]]) and
  * EN-primary report views for a few tests; do not add new content here
  * without regenerating the matching `.cairn`. Not Studio.
  */
object Chemicals:
  private lazy val sdsLanguage: ComposedLanguage =
    PackLoader(EffectContexts.forPackLoader()).requireClosed("sds")
  private lazy val sdsPack = Sds(PackLoader(EffectContexts.forPackLoader()))
  private lazy val typedSectionOrder: List[String] = sdsPack.typedSectionOrder
  private lazy val typedSectionKeys: Map[String, List[String]] = sdsPack.typedSectionKeys

  /** One populated EU-CLP section body. Host [[fields]] stay EN-keyed (for
    * [[SectionReport]]); [[locales]] carry non-EN overlays (`lang` → key → text).
    * [[toTerm]] projects `sectionField` rows for EN then each locale — see SDS
    * `sectionFieldText` / [[SectionFieldStaleness]].
    */
  final case class SectionBody(
      number: Int,
      fields: Map[String, String],
      locales: Map[String, Map[String, String]] = Map.empty,
      /** Non-EN overlays as `fieldLocaleRef` / `sectionFieldRef` into corpus phrases. */
      localeRefs: Map[String, Map[String, String]] = Map.empty,
  ):
    def title: String =
      SectionNumbering.byNumber.getOrElse(number, s"INVALID-$number")
    def outlineEntry: SectionNumbering.OutlineEntry =
      SectionNumbering.OutlineEntry(number, title)

    /** Language term: `eu section N fields ( k lang en : "v", … , k lang fr : "…" )`. */
    def toTerm: Cst =
      val en = fields.toList.map { case (k, v) =>
        Cst.node("sectionField", Cst.Leaf(k), Cst.Leaf("en"), Cst.Leaf(v))
      }
      val loc = locales.toList.flatMap { case (lang, kv) =>
        kv.toList.map { case (k, v) =>
          Cst.node("sectionField", Cst.Leaf(k), Cst.Leaf(lang), Cst.Leaf(v))
        }
      }
      val refs = localeRefs.toList.flatMap { case (lang, kv) =>
        kv.toList.map { case (k, ref) =>
          Cst.node("sectionFieldRef", Cst.Leaf(k), Cst.Leaf(lang), Cst.Leaf(ref))
        }
      }
      Cst.node("euSection", Cst.Leaf(number.toString), Cst.Node("list", en ++ loc ++ refs))

    private def localeField(
        overlays: Map[String, Map[String, String]],
        refs: Map[String, Map[String, String]],
    ): Cst =
      val free = overlays.toList.flatMap { case (lang, kv) =>
        kv.toList.map { case (k, v) =>
          Cst.node("fieldLocale", Cst.Leaf(k), Cst.Leaf(lang), Cst.Leaf(v))
        }
      }
      val refRows = refs.toList.flatMap { case (lang, kv) =>
        kv.toList.map { case (k, ref) =>
          Cst.node("fieldLocaleRef", Cst.Leaf(k), Cst.Leaf(lang), Cst.Leaf(ref))
        }
      }
      val rows = free ++ refRows
      if rows.isEmpty then Cst.Node("none", Nil)
      else Cst.Node("some", List(Cst.Node("list", rows)))

    /** Typed EU-CLP section from surface slot order; unknown numbers stay `euSection`. */
    def toTypedTerm: Cst =
      typedSectionOrder.lift(number - 1) match
        case None => toTerm
        case Some(tag) =>
          val keys = typedSectionKeys.getOrElse(tag, Nil)
          val leaves = keys.map { k =>
            Cst.Leaf(fields.getOrElse(k, sys.error(s"$tag missing EN key '$k'")))
          }
          Cst.Node(tag, leaves :+ localeField(locales, localeRefs))

  /** A chemical document as a (possibly sparse) map of section bodies. */
  final case class ChemicalDoc(
      name: String,
      cas: String,
      sections: Map[Int, SectionBody],
      /** Module-level corpus phrases: (defName, phraseName, lang, text). */
      corpus: List[(String, String, String, String)] = Nil,
  ):
    def outline: List[SectionNumbering.OutlineEntry] =
      sections.keys.toList.sorted.map(n => sections(n).outlineEntry)

    def populatedNumbers: List[Int] = sections.keys.toList.sorted

    def validateOutline: Either[List[SectionNumbering.OutlineError], List[SectionNumbering.SectionDef]] =
      SectionNumbering.validateOutline(outline)

    /** Stable def name for section `n` inside [[toModule]]. */
    def sectionRef(n: Int): String = s"s$n"

    private def corpusDefs: List[(String, Cst)] =
      corpus.map { case (defName, phraseName, lang, text) =>
        defName -> Cst.node("corpusPhrase", Cst.Leaf(phraseName), Cst.Leaf(lang), Cst.Leaf(text))
      }

    /** Project into SDS language terms: one `euSection` per populated number
      * plus a named `outline` binding section refs in ascending order.
      * Outline section-list uses the surface `opt sepby1` shape (`some`/`none`).
      */
    def toModule(outlineName: String = "docOutline"): Module =
      val secDefs = populatedNumbers.map { n =>
        sectionRef(n) -> sections(n).toTerm
      }
      val refs = populatedNumbers.map(n => Cst.Leaf(sectionRef(n)))
      val sectionsField =
        if refs.isEmpty then Cst.Node("none", Nil)
        else Cst.Node("some", List(Cst.Node("list", refs)))
      val outlineTerm = Cst.node(
        "outline",
        Cst.Leaf(name),
        Cst.Leaf(cas),
        sectionsField)
      Module(corpusDefs ++ secDefs :+ (outlineName -> outlineTerm)).sorted

    /** Like [[toModule]] but all populated 1–16 sections use typed ctors. */
    def toTypedModule(outlineName: String = "docOutline"): Module =
      val secDefs = populatedNumbers.map { n =>
        sectionRef(n) -> sections(n).toTypedTerm
      }
      val refs = populatedNumbers.map(n => Cst.Leaf(sectionRef(n)))
      val sectionsField =
        if refs.isEmpty then Cst.Node("none", Nil)
        else Cst.Node("some", List(Cst.Node("list", refs)))
      val outlineTerm = Cst.node(
        "outline",
        Cst.Leaf(name),
        Cst.Leaf(cas),
        sectionsField)
      Module(corpusDefs ++ secDefs :+ (outlineName -> outlineTerm)).sorted

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
          "ld50Oral" -> "5800 mg/kg (rat, oral)",
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

    /** Full 16-section acetone from `languages/sds/chemicals/acetone.cairn`. */
    def asModule: Module =
      ChemicalSource.acetone(sdsLanguage).fold(e => throw RuntimeException(e), identity)

    /** Deeper FR overlays across identification / hazards / composition /
      * physical / toxicological / transport / other — demonstration
      * translations, not a regulatory filing. Untranslated EN keys still
      * resolve via `sectionFieldText` fallback. Standardized GHS signal-word
      * uses [[ghsCorpus]] + `fieldLocaleRef` (see [[thin]]).
      */
    val thinFr: Map[Int, Map[String, String]] = Map(
      1 -> Map(
        "productName" -> "Acétone",
        "synonyms" -> "propan-2-one ; 2-propanone",
        "recommendedUse" ->
          "Réactif de laboratoire ; solvant pour revêtements, adhésifs et nettoyage.",
        "usesAdvisedAgainst" ->
          "Utilisation alimentaire, médicamenteuse, pesticide ou biocide.",
        "supplierName" ->
          "Cairn Chemicals Demo Ltd. (fictionnel, à des fins de démonstration uniquement)",
        "emergencyPhone" -> "+32 000 000 000 (numéro de démonstration)"),
      2 -> Map(
        "classificationSummary" ->
          "Liquide inflammable catégorie 2 ; lésions oculaires graves/irritation oculaire catégorie 2 ; STOT SE catégorie 3 (SNC) — règlement (CE) n° 1272/2008",
        "hazardsNotOtherwiseClassified" ->
          "Une exposition répétée peut provoquer dessèchement ou gerçures de la peau.",
        "hazardPhrases" -> "H225 ; H319 ; H336",
        "pictograms" -> "GHS02 ; GHS07"),
      3 -> Map(
        "componentName" -> "Acétone",
        "concentration" -> ">= 99,5 %"),
      9 -> Map(
        "appearance" -> "Liquide incolore.",
        "odor" -> "Caractéristique, légèrement sucré (type menthe)."),
      11 -> Map(
        "irritation" ->
          "Provoque une sévère irritation des yeux. Un contact cutané prolongé peut provoquer un dessèchement."),
      14 -> Map(
        "properShippingName" -> "ACÉTONE"),
      16 -> Map(
        "otherInformation" ->
          "Données de démonstration / exemple pour le corpus chimiques SDS Cairn — pas un conseil réglementaire ou de sécurité pour un usage réel."))

    /** Shared GHS corpus phrases referenced by thin typed sections.
      * Primary (EN) def name matches the phrase Name so `fieldLocaleRef`
      * validate's `defined(ref)` hits (same pattern as SdsTutorial `h225`).
      */
    val ghsCorpus: List[(String, String, String, String)] = List(
      ("ghsDanger", "ghsDanger", "en", "Danger"),
      ("ghsDangerFr", "ghsDanger", "fr", "Danger"),
      ("ghsDangerDe", "ghsDanger", "de", "Gefahr"))

    /** Thin subset with FR free-text + corpus-ref signalWord (typed 1/2/3/9/11/14/16). */
    val thin: ChemicalDoc = ChemicalDoc(
      name = "Acetone",
      cas = "67-64-1",
      corpus = ghsCorpus,
      sections = Map(
        1 -> pure.sections(1).copy(locales = Map("fr" -> thinFr(1))),
        2 -> pure.sections(2).copy(
          locales = Map("fr" -> thinFr(2)),
          localeRefs = Map("fr" -> Map("signalWord" -> "ghsDanger"))),
        3 -> pure.sections(3).copy(locales = Map("fr" -> thinFr(3))),
        9 -> pure.sections(9).copy(locales = Map("fr" -> thinFr(9))),
        11 -> pure.sections(11).copy(locales = Map("fr" -> thinFr(11))),
        14 -> pure.sections(14).copy(locales = Map("fr" -> thinFr(14))),
        16 -> pure.sections(16).copy(locales = Map("fr" -> thinFr(16)))))

    def thinModule: Module =
      ChemicalSource.acetoneThin(sdsLanguage).fold(e => throw RuntimeException(e), identity)

  /** Ethanol: secondary chemical with fuller EU-CLP outline (all 16 sections).
    * Content is demonstration / literature-shaped placeholder — not a filing.
    * Thin FR overlays mirror acetone's multilingual / restale slice.
    */
  object Ethanol:
    val pure: ChemicalDoc = ChemicalDoc(
      name = "Ethanol",
      cas = "64-17-5",
      sections = Map(
        1 -> SectionBody(1, Map(
          "productName" -> "Ethanol",
          "synonyms" -> "Ethyl alcohol; ethanol absolute; spirit of wine",
          "recommendedUse" -> "Laboratory reagent; solvent; disinfectant formulation feedstock.",
          "usesAdvisedAgainst" -> "Food, drug, or biocidal product use without further authorisation.",
          "supplierName" -> "Cairn Chemicals Demo Ltd. (fictional, for demonstration only)",
          "emergencyPhone" -> "+32 000 000 000 (demo number)")),
        2 -> SectionBody(2, Map(
          "classificationSummary" ->
            "Flammable liquid Category 2; Serious eye damage/eye irritation Category 2 — Regulation (EC) No 1272/2008",
          "hazardsNotOtherwiseClassified" -> "Highly flammable liquid and vapour; vapours may form explosive mixtures with air.",
          "hazardPhrases" -> "H225; H319",
          "signalWord" -> "Danger",
          "pictograms" -> "GHS02; GHS07")),
        3 -> SectionBody(3, Map(
          "componentName" -> "Ethanol",
          "cas" -> "64-17-5",
          "ec" -> "200-578-6",
          "concentration" -> ">= 99.5 %")),
        4 -> SectionBody(4, Map(
          "generalAdvice" -> "If symptoms persist, call a physician.",
          "inhalation" -> "Remove to fresh air. If breathing is difficult, give oxygen.",
          "skinContact" -> "Wash off with soap and plenty of water. Remove contaminated clothing.",
          "eyeContact" -> "Rinse cautiously with water for several minutes. Remove contact lenses if present.",
          "ingestion" -> "Rinse mouth. Do NOT induce vomiting.")),
        5 -> SectionBody(5, Map(
          "extinguishingMedia" -> "Alcohol-resistant foam, dry chemical, or carbon dioxide.",
          "unsuitableExtinguishingMedia" -> "Water jet may spread the fire.",
          "specialHazards" -> "Vapours may travel to ignition source and flash back; containers may rupture when heated.",
          "firefighterProtection" -> "Wear SCBA and full protective gear.")),
        6 -> SectionBody(6, Map(
          "personalPrecautions" -> "Remove ignition sources. Ventilate. Wear appropriate PPE.",
          "environmentalPrecautions" -> "Prevent entry into drains and waterways.",
          "cleanupMethods" -> "Contain spill; soak up with inert absorbent; dispose as hazardous waste.")),
        7 -> SectionBody(7, Map(
          "handling" -> "Keep away from heat, sparks, and open flames. Use only in well-ventilated areas.",
          "storage" -> "Store cool, well-ventilated, away from ignition sources. Keep container tightly closed.",
          "storageIncompatibilities" -> "Strong oxidizers, alkali metals, strong acids, peroxides.")),
        8 -> SectionBody(8, Map(
          "occupationalExposureLimit" -> "EU indicative OELV 1000 ppm (1920 mg/m3), 8-hour TWA; ACGIH STEL 1000 ppm.",
          "engineeringControls" -> "Adequate ventilation; explosion-proof electrical equipment; eyewash nearby.",
          "eyeProtection" -> "Chemical safety goggles (e.g. EN166).",
          "skinProtection" -> "Protective gloves and clothing.",
          "respiratoryProtection" -> "Organic vapor respirator if exposure limits exceeded.")),
        9 -> SectionBody(9, Map(
          "appearance" -> "Colourless liquid.",
          "odor" -> "Characteristic, alcoholic.",
          "molecularWeight" -> "46.07 g/mol",
          "meltingPoint" -> "-114.1 degC",
          "boilingPoint" -> "78.3 degC at 1013 hPa",
          "flashPoint" -> "13 degC (closed cup)",
          "density" -> "0.79 g/cm3 at 20 degC",
          "solubility" -> "Miscible with water in all proportions.",
          "explosiveLimits" -> "Lower 3.3% / Upper 19% (v/v in air)")),
        10 -> SectionBody(10, Map(
          "stability" -> "Stable under recommended storage conditions.",
          "conditionsToAvoid" -> "Heat, flames, sparks, and static discharge.",
          "incompatibleMaterials" -> "Strong oxidizing agents, alkali metals, strong acids, peroxides.",
          "hazardousDecomposition" -> "CO, CO2 under fire conditions.")),
        11 -> SectionBody(11, Map(
          "ld50Oral" -> "7060 mg/kg (rat, oral)",
          "irritation" -> "Causes serious eye irritation. Prolonged skin contact may cause dryness.",
          "inhalationEffects" -> "High vapour concentrations may cause dizziness and narcosis.",
          "carcinogenicity" -> "IARC Group 1 for alcoholic beverages; ethanol as industrial substance not classified as a carcinogen in this demo note.")),
        12 -> SectionBody(12, Map(
          "ecotoxicity" -> "Fish LC50 ~14200 mg/L (96h); daphnia EC50 ~9268 mg/L (48h).",
          "persistence" -> "Readily biodegradable; persistence unlikely.",
          "bioaccumulation" -> "Low (log Pow = -0.31).",
          "mobility" -> "Likely mobile in soil and water due to miscibility.")),
        13 -> SectionBody(13, Map(
          "disposalMethods" -> "Dispose per local/regional/national regulations. Do not discharge to drains.",
          "wasteClassification" -> "Consult local regulations; flammable liquid waste stream.")),
        14 -> SectionBody(14, Map(
          "unNumber" -> "UN1170",
          "properShippingName" -> "ETHANOL (ETHYL ALCOHOL) or ETHANOL SOLUTION",
          "transportHazardClass" -> "Class 3 (Flammable liquid)",
          "packingGroup" -> "II")),
        15 -> SectionBody(15, Map(
          "regulatoryInfo" -> "Classified and labelled per Regulation (EC) No 1272/2008 (CLP).",
          "reachStatus" -> "Registered under REACH (demo note).",
          "usInventory" -> "Listed on US TSCA Inventory (active).")),
        16 -> SectionBody(16, Map(
          "revisionDate" -> "2026-07-22",
          "otherInformation" ->
            "Demo/example data for the Cairn SDS chemicals corpus (secondary ethanol) — not regulatory or safety advice for real use."))))

    def outline: List[SectionNumbering.OutlineEntry] = pure.outline

    /** Full 16-section ethanol as ΔSDS-editable `euSection` + `outline` terms. */
    def asModule: Module =
      ChemicalSource.ethanol(sdsLanguage).fold(e => throw RuntimeException(e), identity)

    /** Deeper FR overlays (mirrors acetone multilingual slice). */
    val thinFr: Map[Int, Map[String, String]] = Map(
      1 -> Map(
        "productName" -> "Éthanol",
        "synonyms" -> "alcool éthylique ; éthanol absolu",
        "recommendedUse" ->
          "Réactif de laboratoire ; solvant ; matière première pour formulations désinfectantes.",
        "usesAdvisedAgainst" ->
          "Utilisation alimentaire, médicamenteuse ou biocide sans autorisation complémentaire.",
        "supplierName" ->
          "Cairn Chemicals Demo Ltd. (fictionnel, à des fins de démonstration uniquement)",
        "emergencyPhone" -> "+32 000 000 000 (numéro de démonstration)"),
      2 -> Map(
        "classificationSummary" ->
          "Liquide inflammable catégorie 2 ; lésions oculaires graves/irritation oculaire catégorie 2 — règlement (CE) n° 1272/2008",
        "hazardsNotOtherwiseClassified" ->
          "Liquide et vapeurs extrêmement inflammables ; les vapeurs peuvent former des mélanges explosifs avec l'air.",
        "hazardPhrases" -> "H225 ; H319",
        "pictograms" -> "GHS02 ; GHS07"),
      3 -> Map(
        "componentName" -> "Éthanol",
        "concentration" -> ">= 99,5 %"),
      9 -> Map(
        "appearance" -> "Liquide incolore.",
        "odor" -> "Caractéristique, alcoolique."),
      11 -> Map(
        "irritation" ->
          "Provoque une sévère irritation des yeux. Un contact cutané prolongé peut provoquer un dessèchement."),
      14 -> Map(
        "properShippingName" -> "ÉTHANOL (ALCOOL ÉTHYLIQUE) ou ÉTHANOL EN SOLUTION"),
      16 -> Map(
        "otherInformation" ->
          "Données de démonstration / exemple pour le corpus chimiques SDS Cairn (éthanol secondaire) — pas un conseil réglementaire ou de sécurité pour un usage réel."))

    /** Optional DE overlays (second non-FR language) on identification + hazards. */
    val thinDe: Map[Int, Map[String, String]] = Map(
      1 -> Map(
        "productName" -> "Ethanol",
        "synonyms" -> "Ethylalkohol ; absolutes Ethanol"),
      2 -> Map(
        "hazardPhrases" -> "H225 ; H319",
        "pictograms" -> "GHS02 ; GHS07"))

    val ghsCorpus: List[(String, String, String, String)] = Acetone.ghsCorpus

    /** Thin subset with FR + DE free-text and corpus-ref signalWord. */
    val thin: ChemicalDoc = ChemicalDoc(
      name = "Ethanol",
      cas = "64-17-5",
      corpus = ghsCorpus,
      sections = Map(
        1 -> pure.sections(1).copy(locales = Map(
          "fr" -> thinFr(1),
          "de" -> thinDe(1))),
        2 -> pure.sections(2).copy(
          locales = Map("fr" -> thinFr(2), "de" -> thinDe(2)),
          localeRefs = Map(
            "fr" -> Map("signalWord" -> "ghsDanger"),
            "de" -> Map("signalWord" -> "ghsDanger"))),
        3 -> pure.sections(3).copy(locales = Map("fr" -> thinFr(3))),
        9 -> pure.sections(9).copy(locales = Map("fr" -> thinFr(9))),
        11 -> pure.sections(11).copy(locales = Map("fr" -> thinFr(11))),
        14 -> pure.sections(14).copy(locales = Map("fr" -> thinFr(14))),
        16 -> pure.sections(16).copy(locales = Map("fr" -> thinFr(16)))))

    def thinModule: Module =
      ChemicalSource.ethanolThin(sdsLanguage).fold(e => throw RuntimeException(e), identity)

    /** Pre-expansion sparse 1+2 outline retained for contrast with the fuller pack. */
    val sparseLegacy: ChemicalDoc = ChemicalDoc(
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
