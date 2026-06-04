import plotly.graph_objects as go
import plotly.io as pio

TERRACOTTA = "#C4603A"
SAGE       = "#7A8C6E"
TEXT_PRI   = "#2C2C2C"
TEXT_SEC   = "#6B6B6B"
GRID       = "#E0D9D0"
BG         = "#FAF7F2"

_template = go.layout.Template()
_template.layout.update(
    paper_bgcolor=BG,
    plot_bgcolor=BG,
    font=dict(color=TEXT_PRI, family="sans-serif"),
    colorway=[TERRACOTTA, SAGE, "#8B7355", "#5B8FA8", "#D4956A"],
    xaxis=dict(gridcolor=GRID, gridwidth=1, showline=False, zeroline=False),
    yaxis=dict(gridcolor=GRID, gridwidth=1, showline=False, zeroline=False),
    margin=dict(t=40, b=40, l=40, r=20),
)

pio.templates["portugal"] = _template
pio.templates.default    = "plotly_white+portugal"
