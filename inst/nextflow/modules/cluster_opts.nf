// Shared clusterOptions builder for the scheduler-facing processes: SGE needs an
// explicit login shell, plus whatever extra directives the site config supplies.
// cfg is a params section (e.g. params.annotate).
def clusterOpts(Map cfg) {
    def opts = [
        (cfg.executor == 'sge') ? '-S /bin/bash' : '',
        (cfg.clusterOptions instanceof String) ? cfg.clusterOptions : ''
    ].findAll { it }.join(' ')
    opts ?: null
}
