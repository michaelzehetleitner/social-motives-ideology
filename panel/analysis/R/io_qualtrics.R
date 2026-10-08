# Reading and writing Qualtrics SAV exports
#
# The raw schema below is the 97-column layout of the Qualtrics export of the
# ZM-ASC panel survey. Column names, labels and ImportId strings were taken
# verbatim from the header rows of the live CSV export (names / labels /
# ImportId JSON); the two `Last_Seen_*` columns exist only in the SAV
# template export and are therefore optional (`required = FALSE`). Their
# ImportId strings are not observable in the CSV and are placeholders.
# Names are normalised with zm_normalise_names() (R/config.R): every
# character that is not a letter, digit or underscore becomes `_`, which maps
# `SDO-D_1` -> `SDO_D_1` and `Duration (in seconds)` -> `Duration__in_seconds_`,
# i.e. the CSV names onto the SAV names and onto the codebook item codes.

#' Qualtrics column names of the raw export, in export order
#'
#' @return Character vector of length 97 (the original, un-normalised names).
zm_raw_qualtrics_names <- function() {
  c("StartDate", "EndDate", "Status", "Progress", "Duration (in seconds)", 
    "Finished", "RecordedDate", "ResponseId", "DistributionChannel", 
    "UserLanguage", "Q_BallotBoxStuffing", "Last_Seen_Flow_Element_ID", 
    "Last_Seen_Question_IDs", "info_teilnahme_2_1", "consent_check", 
    "demo_age", "pol_symp_spd", "pol_symp_cdu_csu", "pol_symp_greens", 
    "pol_symp_fdp", "pol_symp_afd", "pol_symp_linke", "UMS_ach_1", 
    "UMS_ach_2", "UMS_ach_3", "UMS_int_1", "UMS_int_2", "attentioncheck_1", 
    "UMS_ach_4", "UMS_ach_5", "UMS_ach_6", "UMS_int_5", "DOPL_pre_5", 
    "DOPL_pre_6", "UMS_int_3", "UMS_int_4", "UMS_int_6", "DOPL_dom_1", 
    "DOPL_dom_2", "DOPL_dom_3", "DOPL_dom_4", "DOPL_dom_5", "DOPL_dom_6", 
    "DOPL_pre_1", "DOPL_pre_2", "DOPL_pre_3", "DOPL_pre_4", "UNT_1_r", 
    "UNT_2", "UNT_3", "UNT_4_r", "UNT_5", "UNT_6", "ASC_asu_1", "ASC_asu_2", 
    "ASC_asu_3_r", "ASC_asu_4", "ASC_asu_5_r", "ASC_asu_6_r", "ASC_con_1_r", 
    "ASC_con_2", "ASC_con_3", "ASC_con_4", "ASC_con_5_r", "ASC_con_6_r", 
    "ASC_con_7", "ASC_aag_1", "ASC_aag_2", "ASC_aag_3_r", "ASC_aag_4_r", 
    "ASC_aag_5_r", "ASC_aag_6", "SDO-D_1", "SDO-D_2", "SDO-D_3", 
    "SDO-D_4", "SDO-D_5_r", "SDO-D_6_r", "SDO-D_7_r", "SDO-D_8_r", 
    "attentioncheck_2", "pol_vote_would", "pol_party_vote", "pol_party_vote_801_TEXT", 
    "pol_left_right", "demo_gender", "demo_edu_school", "demo_bundesland", 
    "demo_income_hh_net", "demo_hh_members", "Kommentarfeld", "bilendi_id", 
    "survey_status", "survey_status_detail", "possibly_left", "possibly_conservative", 
    "quota_group")
}

#' Variable labels of the raw export (second header row)
#'
#' @return Character vector of length 97.
zm_raw_labels <- function() {
  c("Start Date", "End Date", "Response Type", "Progress", "Duration (in seconds)", 
    "Finished", "Recorded Date", "Response ID", "Distribution Channel", 
    "User Language", "Q_BallotBoxStuffing", "Last Seen Flow Element ID", 
    "Last Seen Question IDs", "Wenn Sie die detaillierten Informationen zum Umgang mit personenbezogenen Daten anzeigen möchten, wählen Sie bitte Anzeigen. - Anzeigen", 
    "Bitte wählen Sie eine der folgenden Optionen:", "Wie alt sind Sie in Jahren?", 
    "Was halten Sie von der SPD?", "Was halten Sie von CDU/CSU?", 
    "Was halten Sie von den Grünen?", "Was halten Sie von der FDP?", 
    "Was halten Sie von der AfD?", "Was halten Sie von der Linken?", 
    "Wie wichtig ist Ihnen folgendes Ziel?  Meine Leistung stets auf einem hohem Niveau zu halten, ist ...", 
    "Wie wichtig ist Ihnen folgendes Ziel?  Arbeit von hoher Qualität zu leisten, ist ...", 
    "Wie wichtig ist Ihnen folgendes Ziel? Projekte, die mich bis an die Grenze meiner Leistungsfähigkeit bringen, sind ...", 
    "Wie wichtig ist Ihnen folgendes Ziel? Eine tiefgehende Beziehung zu haben,  ist ...", 
    "Wie wichtig ist Ihnen folgendes Ziel? Zuneigung und Liebe zu geben,  ist ...", 
    "Bitte wählen Sie die Antwortoption \"Stimme eher nicht zu\" aus, um zu zeigen, dass Sie diese Frage aufmerksam gelesen haben.", 
    "Wie wichtig ist Ihnen folgendes Ziel?  Mich ständig zu verbessern, ist ...", 
    "Wie wichtig ist Ihnen folgendes Ziel?  Ständig neue, interessante und herausfordernde Ziele und Projekte sind ...", 
    "Wie wichtig ist Ihnen folgendes Ziel?  Verantwortung für schwierige und herausfordernde Aufgaben und Ziele zu übernehmen, ist ...", 
    "Wie wichtig ist Ihnen folgendes Ziel?  Nicht von den Menschen getrennt zu sein, die mir wirklich wichtig sind, ist ...", 
    "Wie wichtig ist Ihnen folgendes Ziel?  Anerkennung von anderen Menschen ist ...", 
    "Wie wichtig ist Ihnen folgendes Ziel?  Von anderen Leuten respektiert und bewundert zu werden, ist ...", 
    "Wie sehr stimmen Sie dieser Aussage zu oder nicht?  In einer Partnerschaft wünsche ich mir, vollständig im Anderen aufzugehen.", 
    "Wie sehr stimmen Sie dieser Aussage zu oder nicht?  In einer Partnerschaft wünsche ich mir, alle positiven und negativen Gefühle teilen zu können.", 
    "Wie sehr stimmen Sie dieser Aussage zu oder nicht?  Sich nahezukommen, ist das Einzige, was zählt im Leben.", 
    "Wie sehr stimmen Sie dieser Aussage zu oder nicht?  Ich genieße es, andere meinem Willen zu unterwerfen.", 
    "Wie sehr stimmen Sie dieser Aussage zu oder nicht?  Ich versuche, andere unter meinen Einfluss zu bekommen, anstatt zuzulassen, dass sie mich kontrollieren.", 
    "Wie sehr stimmen Sie dieser Aussage zu oder nicht?  Ich bin bereit, auch aggressive Strategien anzuwenden, um meinen Willen durchzusetzen.", 
    "Wie sehr stimmen Sie dieser Aussage zu oder nicht?  Wenn mich Leute herausfordern, nehme ich in Kauf, sie zu demütigen.", 
    "Wie sehr stimmen Sie dieser Aussage zu oder nicht?  Ich will andere um meinen Finger wickeln.", 
    "Wie sehr stimmen Sie dieser Aussage zu oder nicht?  Ich versuche oft meinen Willen durchzusetzen, unabhängig davon, was andere wollen.", 
    "Wie sehr stimmen Sie dieser Aussage zu oder nicht?  Ich erzähle oft anderen davon, wenn ich etwas Tolles erreicht habe.", 
    "Wie sehr stimmen Sie dieser Aussage zu oder nicht?  Es macht mich traurig, wenn niemand meinen besonderen Fähigkeiten und Talenten Beachtung schenkt.", 
    "Wie sehr stimmen Sie dieser Aussage zu oder nicht?  Erfolg bedeutet, respektiert zu werden.", 
    "Wie sehr stimmen Sie dieser Aussage zu oder nicht?  Es macht mich glücklich, wenn ich anderen meine erfolgreichen Leistungen präsentieren kann.", 
    "Wie sehr stimmen Sie dieser Aussage zu oder nicht?  Wenn ich unbekannte Menschen kennenlerne, löst dies Unbehagen bei mir aus.", 
    "Wie sehr stimmen Sie dieser Aussage zu oder nicht?  Ich finde es reizvoll, in Situationen wie einer Zugfahrt oder im Flugzeug ein Gespräch mit einer mir fremden Person zu beginnen.", 
    "Wie sehr stimmen Sie dieser Aussage zu oder nicht?  Wenn ich längere Zeit nur mit mir vertrauten Menschen verbringe, drängt es mich danach, jemanden Neues kennenzulernen.", 
    "Wie sehr stimmen Sie dieser Aussage zu oder nicht?  Auf Feiern oder ähnlichen gesellschaftlichen Unternehmungen verspüre ich kaum das Bedürfnis, aktiv das Gespräch mit Personen zu suchen, die ich noch nicht kenne.", 
    "Wie sehr stimmen Sie dieser Aussage zu oder nicht?  Wenn ich das Verhalten einer Person noch gar nicht einschätzen kann, löst dies Neugierde in mir aus.", 
    "Wie sehr stimmen Sie dieser Aussage zu oder nicht?  Wenn ich auf einer Veranstaltung bin, auf der sowohl mir bekannte als auch fremde Personen anwesend sind, suche ich aktiv das Gespräch mit Menschen, die ich zuvor noch nicht kennengelernt habe.", 
    "Wie sehr stimmen Sie dieser Aussage zu oder nicht zu? Wir sollten glauben, was unsere führenden Persönlichkeiten sagen.", 
    "Wie sehr stimmen Sie dieser Aussage zu oder nicht zu?  Unsere führenden Persönlichkeiten wissen, was am besten für uns ist.", 
    "Wie sehr stimmen Sie dieser Aussage zu oder nicht zu? Man sollte Aussagen von Autoritätspersonen kritisch sehen.", 
    "Wie sehr stimmen Sie dieser Aussage zu oder nicht zu?  Autoritätspersonen sagen meistens die Wahrheit.", 
    "Wie sehr stimmen Sie dieser Aussage zu oder nicht zu? Man sollte  Aussagen von Autoritätspersonen skeptisch gegenüberstehen.", 
    "Wie sehr stimmen Sie dieser Aussage zu oder nicht zu?  Es ist gut für unsere Gesellschaft, die Motive von Autoritätspersonen zu hinterfragen.", 
    "Wie sehr stimmen Sie dieser Aussage zu oder nicht zu?  Menschen legen allgemein zu viel Wert auf Tradition.", 
    "Wie sehr stimmen Sie dieser Aussage zu oder nicht zu? Traditionen sollten respektiert werden.", 
    "Wie sehr stimmen Sie dieser Aussage zu oder nicht zu?  Traditionen sind die Grundlage einer gesunden Gesellschaft.", 
    "Wie sehr stimmen Sie dieser Aussage zu oder nicht zu?  Es wäre besser für unsere Gesellschaft, wenn mehr Leute soziale Normen einhalten würden.", 
    "Wie sehr stimmen Sie dieser Aussage zu oder nicht zu?  Traditionen behindern den Fortschritt.", 
    "Wie sehr stimmen Sie dieser Aussage zu oder nicht zu? Man sollte gesellschaftliche Traditionen hinterfragen, um unsere Gesellschaft voranzubringen.", 
    "Wie sehr stimmen Sie dieser Aussage zu oder nicht zu? Man sollte sich an soziale Normen halten.", 
    "Wie sehr stimmen Sie dieser Aussage zu oder nicht zu?  Gegen Gruppen, die unsere Gesellschaft bedrohen, muss mit Härte vorgegangen werden.", 
    "Wie sehr stimmen Sie dieser Aussage zu oder nicht zu?  Es ist notwendig, Gewalt gegen Menschen anzuwenden, die eine Bedrohung für die Autorität darstellen.", 
    "Wie sehr stimmen Sie dieser Aussage zu oder nicht zu?  Die Polizei sollte Gewalt gegen Tatverdächtige vermeiden.", 
    "Wie sehr stimmen Sie dieser Aussage zu oder nicht zu?  Man sollte Gewalt gegen andere vermeiden, selbst wenn dies von den zuständigen Behörden angeordnet wird.", 
    "Wie sehr stimmen Sie dieser Aussage zu oder nicht zu?  Die Anwendung von Gewalt gegen Menschen ist falsch, selbst wenn sie von Autoritätspersonen ausgeübt wird.", 
    "Wie sehr stimmen Sie dieser Aussage zu oder nicht zu?  Harte Strafen sind notwendig, um ein Zeichen zu setzen.", 
    "Wie sehr stimmen Sie dieser Aussage zu oder nicht zu?  Manche soziale Gruppen müssen in ihrer gesellschaftlichen Stellung gehalten werden.", 
    "Wie sehr stimmen Sie dieser Aussage zu oder nicht zu?  Es ist wahrscheinlich eine gute Sache, dass bestimmte soziale Gruppen ganz oben und andere ganz unten stehen.", 
    "Wie sehr stimmen Sie dieser Aussage zu oder nicht zu?  Eine ideale Gesellschaft erfordert, dass manche soziale Gruppen ganz oben stehen und andere ganz unten.", 
    "Wie sehr stimmen Sie dieser Aussage zu oder nicht zu?  Manche soziale Gruppen sind einfach weniger wert als andere soziale Gruppen.", 
    "Wie sehr stimmen Sie dieser Aussage zu oder nicht zu?  Sozialen Gruppen ganz unten steht genauso viel zu wie den sozialen Gruppen ganz oben.", 
    "Wie sehr stimmen Sie dieser Aussage zu oder nicht zu?  Keine einzelne soziale Gruppe sollte die Gesellschaft dominieren.", 
    "Wie sehr stimmen Sie dieser Aussage zu oder nicht zu?  Soziale Gruppen ganz unten sollten nicht in ihrer gesellschaftlichen Stellung bleiben müssen.", 
    "Wie sehr stimmen Sie dieser Aussage zu oder nicht zu?  Es ist ein schlechtes Prinzip, wenn eine soziale Gruppe über andere soziale Gruppen bestimmt.", 
    "Bitte wählen Sie die Antwortoption \"Stimme zu\" aus, um zu zeigen, dass Sie diese Frage aufmerksam gelesen haben.", 
    "Wenn am nächsten Sonntag Bundestagswahl wäre, würden Sie wählen gehen?", 
    "Wenn am nächsten Sonntag Bundestagswahl wäre, welche Partei würden Sie wählen? - Selected Choice", 
    "Wenn am nächsten Sonntag Bundestagswahl wäre, welche Partei würden Sie wählen? - Eine andere Partei und zwar – Text", 
    "In der Politik spricht man manchmal von \"links\" und \"rechts\". Wo auf der Skala  würden Sie sich selbst einstufen, wenn 0 für links steht und 10 für rechts?", 
    "Was ist Ihr Geschlecht?", "Welchen höchsten allgemeinbildenden Schulabschluss haben Sie?", 
    "Und in welchem Bundesland wohnen Sie?  Wenn Sie mehrere Wohnsitze haben, geben Sie bitte das Bundesland an, in dem sich Ihr Hauptwohnsitz befindet.", 
    "Wie hoch ist das monatliche Netto-Einkommen Ihres Haushaltes insgesamt? Gemeint ist die Summe, die nach Abzug von Steuern und Sozialversicherungsbeiträgen übrig bleibt.", 
    "Wie viele Personen leben ständig in Ihrem Haushalt, Sie selbst eingeschlossen? Zu diesem Haushalt zählen alle Personen, die hier gemeinsam wohnen und wirtschaften (z. B. vom gleichen Einkommen leben). Denken Sie auch an alle im Haushalt lebenden Kinder.", 
    "Vielen Dank für Ihre Teilnahme! Mit Klick auf den blauen Pfeil ist die Umfrage abgeschlossen.   Wenn Sie uns zuvor noch etwas mitteilen möchten, können Sie folgendes Kommentarfeld gerne dafür nutzen:", 
    "bilendi_id", "survey_status", "survey_status_detail", "possibly_left", 
    "possibly_conservative", "quota_group")
}

#' ImportId JSON strings of the raw export (third header row)
#'
#' @return Character vector of length 97.
zm_raw_import_ids <- function() {
  c("{\"ImportId\":\"startDate\",\"timeZone\":\"Europe/Berlin\"}", 
    "{\"ImportId\":\"endDate\",\"timeZone\":\"Europe/Berlin\"}", 
    "{\"ImportId\":\"status\"}", "{\"ImportId\":\"progress\"}", "{\"ImportId\":\"duration\"}", 
    "{\"ImportId\":\"finished\"}", "{\"ImportId\":\"recordedDate\",\"timeZone\":\"Europe/Berlin\"}", 
    "{\"ImportId\":\"_recordId\"}", "{\"ImportId\":\"distributionChannel\"}", 
    "{\"ImportId\":\"userLanguage\"}", "{\"ImportId\":\"Q_BallotBoxStuffing\"}", 
    "{\"ImportId\":\"lastSeenFlowElementId\"}", "{\"ImportId\":\"lastSeenQuestionIds\"}", 
    "{\"ImportId\":\"QID102\",\"choiceId\":\"1\"}", "{\"ImportId\":\"QID185\"}", 
    "{\"ImportId\":\"QID1219914433_TEXT\"}", "{\"ImportId\":\"QID1219914404\"}", 
    "{\"ImportId\":\"QID1219914405\"}", "{\"ImportId\":\"QID1219914406\"}", 
    "{\"ImportId\":\"QID1219914407\"}", "{\"ImportId\":\"QID1219914408\"}", 
    "{\"ImportId\":\"QID1219914409\"}", "{\"ImportId\":\"QID131\"}", 
    "{\"ImportId\":\"QID132\"}", "{\"ImportId\":\"QID133\"}", "{\"ImportId\":\"QID137\"}", 
    "{\"ImportId\":\"QID138\"}", "{\"ImportId\":\"QID184\"}", "{\"ImportId\":\"QID134\"}", 
    "{\"ImportId\":\"QID135\"}", "{\"ImportId\":\"QID136\"}", "{\"ImportId\":\"QID141\"}", 
    "{\"ImportId\":\"QID162\"}", "{\"ImportId\":\"QID163\"}", "{\"ImportId\":\"QID139\"}", 
    "{\"ImportId\":\"QID140\"}", "{\"ImportId\":\"QID142\"}", "{\"ImportId\":\"QID164\"}", 
    "{\"ImportId\":\"QID165\"}", "{\"ImportId\":\"QID166\"}", "{\"ImportId\":\"QID167\"}", 
    "{\"ImportId\":\"QID168\"}", "{\"ImportId\":\"QID169\"}", "{\"ImportId\":\"QID170\"}", 
    "{\"ImportId\":\"QID171\"}", "{\"ImportId\":\"QID172\"}", "{\"ImportId\":\"QID173\"}", 
    "{\"ImportId\":\"QID149\"}", "{\"ImportId\":\"QID151\"}", "{\"ImportId\":\"QID152\"}", 
    "{\"ImportId\":\"QID153\"}", "{\"ImportId\":\"QID154\"}", "{\"ImportId\":\"QID157\"}", 
    "{\"ImportId\":\"QID1219914411\"}", "{\"ImportId\":\"QID1219914412\"}", 
    "{\"ImportId\":\"QID1219914413\"}", "{\"ImportId\":\"QID1219914414\"}", 
    "{\"ImportId\":\"QID1219914415\"}", "{\"ImportId\":\"QID1219914416\"}", 
    "{\"ImportId\":\"QID1219914417\"}", "{\"ImportId\":\"QID1219914418\"}", 
    "{\"ImportId\":\"QID1219914419\"}", "{\"ImportId\":\"QID1219914420\"}", 
    "{\"ImportId\":\"QID1219914421\"}", "{\"ImportId\":\"QID1219914422\"}", 
    "{\"ImportId\":\"QID1219914423\"}", "{\"ImportId\":\"QID1219914424\"}", 
    "{\"ImportId\":\"QID1219914425\"}", "{\"ImportId\":\"QID1219914426\"}", 
    "{\"ImportId\":\"QID1219914427\"}", "{\"ImportId\":\"QID1219914428\"}", 
    "{\"ImportId\":\"QID1219914429\"}", "{\"ImportId\":\"QID1220313271\"}", 
    "{\"ImportId\":\"QID1220313272\"}", "{\"ImportId\":\"QID1220313273\"}", 
    "{\"ImportId\":\"QID1220313274\"}", "{\"ImportId\":\"QID1220313275\"}", 
    "{\"ImportId\":\"QID1220313276\"}", "{\"ImportId\":\"QID1220313277\"}", 
    "{\"ImportId\":\"QID1220313278\"}", "{\"ImportId\":\"QID1219914430\"}", 
    "{\"ImportId\":\"QID1219923304\"}", "{\"ImportId\":\"QID1219923305\"}", 
    "{\"ImportId\":\"QID1219923305_8_TEXT\"}", "{\"ImportId\":\"QID1219923306\"}", 
    "{\"ImportId\":\"QID1219914432\"}", "{\"ImportId\":\"QID1219914434\"}", 
    "{\"ImportId\":\"QID1219914435\"}", "{\"ImportId\":\"QID1219914436\"}", 
    "{\"ImportId\":\"QID1219914437\"}", "{\"ImportId\":\"QID105_TEXT\"}", 
    "{\"ImportId\":\"bilendi_id\"}", "{\"ImportId\":\"survey_status\"}", 
    "{\"ImportId\":\"survey_status_detail\"}", "{\"ImportId\":\"possibly_left\"}", 
"{\"ImportId\":\"possibly_conservative\"}", "{\"ImportId\":\"quota_group\"}"
)
}

#' Expected schema of the raw export
#'
#' One row per expected column, in export order. `name` is the normalised
#' name used throughout the pipeline, `qualtrics_name` the name in a CSV
#' export, `type` the storage type after [read_qualtrics_export()]
#' (`datetime`, `character`, `integer`, `numeric`), `role` one of
#' meta / flow / consent / age / politics / item / attention / demo / comment,
#' `required` whether [assert_raw_schema()] insists on the column, `label`
#' and `import_id` the second and third header rows of a CSV export.
#'
#' @return Tibble with 97 rows and columns name, qualtrics_name, type, role,
#'   required, label, import_id.
zm_raw_schema <- function() {
  qualtrics_name <- zm_raw_qualtrics_names()
  name <- zm_normalise_names(qualtrics_name)

  datetime_cols <- c("StartDate", "EndDate", "RecordedDate")
  meta_chr <- c(
    "Status", "ResponseId", "DistributionChannel", "UserLanguage",
    "Q_BallotBoxStuffing", "Last_Seen_Flow_Element_ID", "Last_Seen_Question_IDs"
  )
  meta_int <- c("Progress", "Finished")
  flow_cols <- c(
    "bilendi_id", "survey_status", "survey_status_detail",
    "possibly_left", "possibly_conservative", "quota_group"
  )
  consent_cols <- c("info_teilnahme_2_1", "consent_check")
  politics_num <- c(
    "pol_symp_spd", "pol_symp_cdu_csu", "pol_symp_greens", "pol_symp_fdp",
    "pol_symp_afd", "pol_symp_linke", "pol_vote_would", "pol_party_vote",
    "pol_left_right"
  )
  demo_cols <- c(
    "demo_gender", "demo_edu_school", "demo_bundesland",
    "demo_income_hh_net", "demo_hh_members"
  )
  attention_cols <- c("attentioncheck_1", "attentioncheck_2")
  item_pattern <- "^(UMS_|DOPL_|UNT_|ASC_|SDO_D_)"

  role <- rep(NA_character_, length(name))
  role[name %in% c(datetime_cols, meta_chr, meta_int, "Duration__in_seconds_")] <- "meta"
  role[name %in% flow_cols] <- "flow"
  role[name %in% consent_cols] <- "consent"
  role[name == "demo_age"] <- "age"
  role[name %in% c(politics_num, "pol_party_vote_801_TEXT")] <- "politics"
  role[grepl(item_pattern, name)] <- "item"
  role[name %in% attention_cols] <- "attention"
  role[name %in% demo_cols] <- "demo"
  role[name == "Kommentarfeld"] <- "comment"
  if (anyNA(role)) {
    stop("zm_raw_schema(): no role for ", paste(name[is.na(role)], collapse = ", "))
  }

  type <- rep("numeric", length(name))
  type[name %in% datetime_cols] <- "datetime"
  type[name %in% c(meta_chr, flow_cols, "pol_party_vote_801_TEXT", "Kommentarfeld")] <- "character"
  type[name %in% meta_int] <- "integer"

  tibble::tibble(
    name = name,
    qualtrics_name = qualtrics_name,
    type = type,
    role = role,
    required = !(name %in% c("Last_Seen_Flow_Element_ID", "Last_Seen_Question_IDs")),
    label = zm_raw_labels(),
    import_id = zm_raw_import_ids()
  )
}

#' Read a Qualtrics SAV export
#'
#' Returns a tibble with normalised column names and the storage types of
#' [zm_raw_schema()]: item, scalometer, demographic and attention-check
#' columns numeric; flow columns character; `Finished` and `Progress`
#' integer; the three timestamps POSIXct (Europe/Berlin). `Status` stays as
#' exported (a code or a labelled value; see [zm_status_label()]).
#' Columns not in the schema are kept unchanged after the schema columns.
#' The attribute `labels` holds a named list variable -> label.
#'
#' An export that carries its columns but no data row stops here: a download
#' that returned no responses would otherwise travel silently through AP1 and
#' only break deep inside the standardisation.
#'
#' @param path Path to a `.sav` export.
#' @return Tibble.
read_qualtrics_export <- function(path) {
  if (!file.exists(path)) stop("Export not found: ", path)
  ext <- tolower(tools::file_ext(path))
  if (ext != "sav") {
    stop("Unsupported export format '.", ext, "' (expected .sav): ", path)
  }
  out <- zm_read_qualtrics_sav(path)
  if (nrow(out) == 0) {
    stop(
      "read_qualtrics_export(): the export has no data row (", ncol(out),
      " column(s), 0 rows): ", path,
      ". An empty export cannot be analysed; check the download."
    )
  }
  out
}

#' Read the SAV export format
#'
#' @param path Path to the SAV file.
#' @return Tibble as described in [read_qualtrics_export()].
zm_read_qualtrics_sav <- function(path) {
  data <- haven::read_sav(path)
  names(data) <- zm_normalise_names(names(data))
  labels <- lapply(data, function(x) {
    lab <- attr(x, "label", exact = TRUE)
    if (is.null(lab)) NA_character_ else as.character(lab)
  })
  zm_coerce_export(data, labels)
}

#' Coerce export columns to the schema types
#'
#' A value of a schema-numeric column that is not a number (an empty string
#' and `NA` aside) stops the pipeline, naming the column: it means the export
#' carries choice text where the recoded numbers belong.
#'
#' @param data Tibble with normalised names: haven-labelled, numeric or
#'   character columns, as haven reads a SAV export.
#' @param labels Named list variable -> label.
#' @return Tibble with schema types, schema column order, `labels` attribute.
zm_coerce_export <- function(data, labels) {
  schema <- zm_raw_schema()
  tz <- "Europe/Berlin"

  to_datetime <- function(x) {
    if (inherits(x, "POSIXct")) {
      # haven stamps SAV datetimes as UTC although they are wall-clock times.
      return(as.POSIXct(format(x, "%Y-%m-%d %H:%M:%S", tz = "UTC"), tz = tz))
    }
    x <- as.character(x)
    x[!nzchar(x)] <- NA_character_
    as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S", tz = tz)
  }
  to_numeric <- function(x, name) {
    if (inherits(x, "haven_labelled")) x <- haven::zap_labels(x)
    if (is.numeric(x)) return(as.numeric(x))
    x <- trimws(as.character(x))
    x[x %in% c("", "NA")] <- NA_character_
    value <- suppressWarnings(as.numeric(x))
    failed <- !is.na(x) & is.na(value)
    if (any(failed)) {
      shown <- unique(x[failed])
      stop(
        "Column '", name, "' of the export has ", sum(failed),
        " value(s) that are not numbers: ",
        paste(utils::head(shown, 5), collapse = ", "),
        if (length(shown) > 5) ", ..." else ""
      )
    }
    value
  }
  to_integer <- function(x, name) as.integer(to_numeric(x, name))
  to_character <- function(x) {
    if (inherits(x, "haven_labelled")) return(x)
    x <- as.character(x)
    x[is.na(x)] <- ""
    x
  }

  for (i in seq_len(nrow(schema))) {
    nm <- schema$name[i]
    if (!nm %in% names(data)) next
    data[[nm]] <- switch(
      schema$type[i],
      datetime = to_datetime(data[[nm]]),
      integer = to_integer(data[[nm]], nm),
      numeric = to_numeric(data[[nm]], nm),
      character = to_character(data[[nm]])
    )
  }
  present <- intersect(schema$name, names(data))
  extra <- setdiff(names(data), schema$name)
  data <- tibble::as_tibble(data[, c(present, extra), drop = FALSE])
  attr(data, "labels") <- labels[names(data)]
  data
}

#' Assert that an export has the expected columns and types
#'
#' Stops when a required column of [zm_raw_schema()] is missing or when a
#' numeric column (items, attention checks, scalometers, age, demographics,
#' consent) is not numeric.
#'
#' @param df Tibble from [read_qualtrics_export()].
#' @return `df`, invisibly.
assert_raw_schema <- function(df) {
  schema <- zm_raw_schema()
  missing <- setdiff(schema$name[schema$required], names(df))
  if (length(missing) > 0) {
    stop(
      "Raw export lacks ", length(missing), " expected column(s): ",
      paste(missing, collapse = ", ")
    )
  }
  numeric_cols <- schema$name[schema$type == "numeric" & schema$name %in% names(df)]
  bad <- numeric_cols[!vapply(df[numeric_cols], is.numeric, logical(1))]
  if (length(bad) > 0) {
    stop("Raw export has non-numeric values in: ", paste(bad, collapse = ", "))
  }
  # A Qualtrics export can carry choice TEXT instead of the recoded numbers.
  # The reader coerces with as.numeric(), so such a column arrives numeric and
  # entirely NA and would pass the type check above, travel into the scale
  # scores and surface much later as "the exclusions left no respondent". Every
  # item and attention-check column is forced in the survey, so an entirely
  # missing one cannot be a legitimate export of live data.
  forced <- schema$name[schema$role %in% c("item", "attention") &
                          schema$name %in% numeric_cols]
  empty <- forced[vapply(df[forced], function(x) all(is.na(x)), logical(1))]
  if (nrow(df) > 0 && length(empty) > 0) {
    stop(
      "Raw export has no value at all in ", length(empty),
      " forced column(s): ", paste(utils::head(empty, 8), collapse = ", "),
      if (length(empty) > 8) ", …" else "",
      ". These items are forced in the survey, so every value failing to parse ",
      "points to an export in choice text rather than recoded numbers; ",
      "re-export with numeric values."
    )
  }
  invisible(df)
}

#' Write a tibble as a Qualtrics SAV export
#'
#' The counterpart of [zm_read_qualtrics_sav()]: the columns are written under
#' their SAV names with the variable labels, display formats and value labels of
#' a Qualtrics SAV export of the same survey (`template`, the random-data export
#' `data/synthetic/template/ZM-ASC-panel_random-data_2026-09-05.sav`), so that
#' the file reads back through the same route as the real export. Columns the
#' template does not carry are written without labels.
#'
#' @param df Tibble with schema-named columns (see [zm_raw_schema()]).
#' @param path Output path (`.sav`); the directory is created.
#' @param template Path to the SAV export whose column attributes are copied.
#' @return `path`, invisibly.
write_qualtrics_sav <- function(df, path, template) {
  if (!file.exists(template)) stop("SAV template not found: ", template)
  tpl <- haven::read_sav(template, n_max = 0)
  tpl_names <- stats::setNames(names(tpl), zm_normalise_names(names(tpl)))
  out <- lapply(names(df), function(nm) {
    x <- df[[nm]]
    if (inherits(x, "haven_labelled")) x <- haven::zap_labels(x)
    if (is.logical(x)) x <- as.integer(x)
    if (is.factor(x)) x <- as.character(x)
    if (is.character(x)) x[is.na(x)] <- ""
    if (nm %in% names(tpl_names)) {
      t <- tpl[[tpl_names[[nm]]]]
      value_labels <- attr(t, "labels", exact = TRUE)
      if (is.numeric(x) && !is.null(value_labels)) {
        x <- haven::labelled(as.numeric(x), labels = value_labels)
      }
      attr(x, "label") <- attr(t, "label", exact = TRUE)
      fmt <- attr(t, "format.spss", exact = TRUE)
      if (!is.null(fmt) && !inherits(x, "POSIXct")) attr(x, "format.spss") <- fmt
    }
    x
  })
  names(out) <- ifelse(names(df) %in% names(tpl_names), tpl_names[names(df)], names(df))
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  haven::write_sav(tibble::as_tibble(out, .name_repair = "minimal"), path)
  invisible(path)
}

#' Map Qualtrics `Status` codes to their labels
#'
#' The response type arrives as a code (0 = IP Address, 1 = Survey Preview,
#' 2 = Survey Test, ...; the synthetic export carries it so) or as a labelled
#' value (a Qualtrics SAV export). This helper returns the label text in either
#' case and leaves unknown values untouched.
#'
#' @param x The `Status` column as returned by [read_qualtrics_export()].
#' @return Character vector of labels.
zm_status_label <- function(x) {
  codes <- c(
    "0" = "IP Address", "1" = "Survey Preview", "2" = "Survey Test",
    "4" = "Imported", "8" = "Spam", "9" = "Survey Preview Spam",
    "12" = "Imported Spam", "16" = "Offline", "17" = "Offline Survey Preview",
    "32" = "EX", "40" = "EX Spam", "48" = "EX Offline",
    "132" = "Reimported imported", "256" = "Synthetic"
  )
  if (inherits(x, "haven_labelled")) {
    return(as.character(haven::as_factor(x, levels = "labels")))
  }
  x <- as.character(x)
  hit <- x %in% names(codes)
  x[hit] <- codes[x[hit]]
  x
}
