// SQL fragment that returns assemble_notes with any segment starting with the
// given tag (and a preceding '; ' if present) stripped. Used so each stage's
// failure message can be idempotently replaced across re-runs instead of
// accumulating duplicates on -resume.
def stripTagSql(String tag) {
    def lit = tag.replace("'", "''")
    return "RTRIM(" +
        "CASE WHEN INSTR(COALESCE(assemble_notes,''), '${lit}') > 0 " +
            "THEN SUBSTR(COALESCE(assemble_notes,''), 1, INSTR(COALESCE(assemble_notes,''), '${lit}') - 1) " +
            "ELSE COALESCE(assemble_notes,'') END" +
    ", '; ')"
}

// SQL fragment that appends a tagged message to assemble_notes, after first
// stripping any prior segment with the same tag.
def appendTaggedNoteSql(String tag, String msg) {
    def stripped = stripTagSql(tag)
    def tagged = (tag + ' ' + msg).replace("'", "''")
    return "CASE WHEN ${stripped} = '' THEN '${tagged}' ELSE ${stripped} || '; ${tagged}' END"
}
