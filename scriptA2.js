const embedOpt = {
  renderer: "svg",
  config: {
    background: "transparent",
    axis: { labelFont: "Lato", titleFont: "Lato", labelFontSize: 9, titleFontSize: 10, labelColor: "#333", titleColor: "#333" },
    legend: { labelFont: "Lato", titleFont: "Lato", labelFontSize: 9, titleFontSize: 10, labelColor: "#333", titleColor: "#333" },
    title: { font: "Lato", fontSize: 16, color: "#333" },
    style: {
      "guide-label": { font: "Lato" },
      "guide-title": { font: "Lato" },
      "tooltip": { labelFont: "Lato", titleFont: "Lato", labelFontSize: 13, titleFontSize: 13 }
    }
  }
};


const section1MapSpec = {
  "$schema": "https://vega.github.io/schema/vega-lite/v5.json",
  "params": [
    { "name": "yearSelector", "value": 2025 },
    { "name": "selectedEntity", "value": null, "on": [
      {"events": "click", "update": "datum && datum.Entity ? datum.Entity : selectedEntity"},
      {"events": "dblclick", "update": "null"}
    ]}
    
  ],
  "background": "transparent",
  //"title": {"text": "Global Temperature Anomalies (1940–2025) relative to the 1991-2020 baseline", "anchor": "start", "fontSize": 18, "offset": 8},
  "width": 750,
  "height": 420,
  "projection": {"type": "equalEarth", "scale": 125, "translate": [360, 230]},
  "data": {"url": "https://raw.githubusercontent.com/alie0025/DV2_Final/refs/heads/main/country_temp_anomaly_by_year.csv", "format": {"type": "csv"}},
  "transform": [
    {"filter": "datum.Year == yearSelector"},
    {"lookup": "Code", "from": {"data": {"url": "https://raw.githubusercontent.com/alie0025/DV2_Final/refs/heads/main/ne_110m_admin_0_countries.json", "format": {"type": "topojson", "feature": "ne_110m_admin_0_countries"}}, "key": "properties.ADM0_A3"}, "as": "geo"}
  ],
  "layer": [
    {"data": {"url": "https://raw.githubusercontent.com/alie0025/DV2_Final/refs/heads/main/ne_10m_ocean.json", "format": {"type": "topojson", "feature": "ne_10m_ocean"}}, "mark": {"type": "geoshape", "fill": "#dbe6ea"}},
    {"data": {"url": "https://raw.githubusercontent.com/alie0025/DV2_Final/refs/heads/main/ne_110m_graticules_30.json", "format": {"type": "topojson", "feature": "ne_110m_graticules_30"}}, "mark": {"type": "geoshape", "stroke": "#a0a0a0", "strokeWidth": 0.4, "fill": null}},
    {"mark": {"type": "geoshape", "stroke": "white", "strokeWidth": 0.3}, "encoding": {
      "shape": {"field": "geo", "type": "geojson"},
      "color": {"field": "Anomaly", "type": "quantitative", "scale": {"domain": [-1.5, 0, 1.5], "range": ["#004991", "#f7f7f7", "#990012"]}, 
      "legend": {"title": "Temperature anomaly (°C)", "orient": "top", "direction": "horizontal", "gradientLength": 300, "gradientThickness": 10}},
      "stroke": {"condition": [{"test": "selectedEntity && datum.Entity == selectedEntity", "value": "#000"}], "value": "white"},
      "strokeWidth": {"condition": [{"test": "selectedEntity && datum.Entity == selectedEntity", "value": 2.5}], "value": 0.3},
"opacity": {
  "condition": [
    {"test": "selectedEntity && datum.Entity == selectedEntity", "value": 1},
    {"test": "selectedEntity && datum.Entity != selectedEntity", "value": 0.5}
  ],
  "value": 1
},
      "tooltip": [
  {"field": "Entity", "title": "Country"},
  {"field": "Year", "title": "Year"},
  {
    "field": "Anomaly",
    "title": "Anomaly (°C)",
    "type": "quantitative",
    "format": "+.2f"
  }
]
    }},
    {"data": {"url": "https://raw.githubusercontent.com/alie0025/DV2_Final/refs/heads/main/ne_110m_admin_0_countries.json", "format": {"type": "topojson", "feature": "ne_110m_admin_0_countries"}}, "mark": {"type": "geoshape", "fill": null, "stroke": "#333", "strokeWidth": 0.6}},
    {"data": {"sphere": true}, "mark": {"type": "geoshape", "fill": null, "stroke": "#222", "strokeWidth": 1.2}}
  ],
  "config": {
    "background": "transparent",
    "style": {
      "guide-label": {"font": "Lato"},
      "tooltip": {
        "content": "encoding",
        "labelFont": "Lato",
        "labelFontSize": 13,
        "titleFont": "Lato",
        "titleFontWeight": "normal",
        "titleFontSize": 13
      }
    }
  }
};

const section1BarSpec = {
  "$schema": "https://vega.github.io/schema/vega-lite/v5.json",
  "params": [
    { "name": "yearSelector", "value": 2025 },
    { "name": "selectedEntity", "value": "Australia" }
  ],
  "background": "transparent",
  "width": 680,
  "height": 200,
  "data": {"url": "https://raw.githubusercontent.com/alie0025/DV2_Final/refs/heads/main/country_temp_anomaly_by_year.csv", "format": {"type": "csv"}},
  "transform": [{"filter": "selectedEntity ? datum.Entity == selectedEntity : datum.Entity == 'Australia'"}],
  "mark": {"type": "bar", "cornerRadiusTopLeft": 2, "cornerRadiusTopRight": 2},
  "encoding": {
    "x": {"field": "Year", "type": "quantitative", "title": "Year", "scale": {"domain": [1940, 2025]}, "axis": {"labelAngle": 90, "tickCount": 18, "format": "d"}},
    "y": {"field": "Anomaly", "type": "quantitative", "title": "Temperature anomaly (°C)"},
    "color": {"condition": [{"test": "datum.Anomaly > 0", "value": "#990012"}, {"test": "datum.Anomaly < 0", "value": "#004991"}], "value": "#f7f7f7"},
    "tooltip": [{"field": "Year", "title": "Year"}, {"field": "Anomaly", "format": ".2f", "title": "Anomaly (°C)"}]
  },
  "title": {"text": {"signal": "selectedEntity ? selectedEntity + ' Temperature Trend (1940–2025)' : 'Australia’s Temperature Trend (1940–2025)'"}, "anchor": "start", "fontSize": 18, "offset": 8},
  "config": {
    "background": "transparent",
    "style": {
      "guide-label": {"font": "Lato"},
      "tooltip": {
        "content": "encoding",
        "labelFont": "Lato",
        "labelFontSize": 13,
        "titleFont": "Lato",
        "titleFontWeight": "normal",
        "titleFontSize": 13
      }
    }
  }
};

vegaEmbed('#section1-map', section1MapSpec, embedOpt).then(r => {
  window.mapView = r.view;
  // when selection changes on the map, sync to bar
  window.mapView.addSignalListener('selectedEntity', (name, value) => {
    if (window.barView && window.barView.signal) {
      window.barView.signal('selectedEntity', value).run();
    }
  });
});

vegaEmbed('#section1-bar', section1BarSpec, embedOpt).then(r => {
  window.barView = r.view;
});

// Country search functionality
document.getElementById("countrySearch").addEventListener("input", function(event) {
  const countryName = event.target.value.trim(); // Get the value entered by the user
  
  // Find the country matching the search input
  updateSelection(countryName);
});

function updateSelection(countryName) {
  // Check if country name exists in the data (you can do this through a simple lookup)
  const selectedCountry = countryName || "Australia"; // Default to "Australia" if nothing is entered
  
  // Update selectedEntity in the map and bar chart
  if (window.mapView) {
    window.mapView.signal('selectedEntity', selectedCountry).run();
  }
  
  if (window.barView) {
    window.barView.signal('selectedEntity', selectedCountry).run();
  }
}


vegaEmbed("#section2-viz", {
  "$schema": "https://vega.github.io/schema/vega-lite/v5.json",
  //"title": {
   // "text": "Climate Drivers vs Rainfall Anomalies (Australia, 2008–2025)",
   // "anchor": "start",
    //"fontSize": 20
  //},
  "background": "transparent",
  "width": 650,
  "height": 200,

  "data": {
    "url": "https://raw.githubusercontent.com/alie0025/DV2_Final/refs/heads/main/climate_drivers_with_rain.csv",
    "format": {"type": "csv"}
  },

  "transform": [
    {"fold": ["ENSO", "IOD", "SAM"], "as": ["Driver", "Index"]}
  ],

  "layer": [
    {
      "params": [
        {
          "name": "driverHighlight",
          "select": {"type": "point", "fields": ["Driver"]},
          "bind": "legend"
        }
      ],
      "mark": {"type": "area"},
      "encoding": {
        "x": {
          "field": "Year",
          "type": "ordinal",
          "axis": {"labelAngle": 0, "title": "Year", "grid": false}
        },
        "y": {
          "field": "Index",
          "type": "quantitative",
          "axis": {"title": "Climate Driver Index"}
        },
        "color": {
          "field": "Driver",
          "type": "nominal",
          "title": null,
          "scale": {
            "domain": ["ENSO", "IOD", "SAM"],
            "range": ["#dbc589", "#ec9d87", "#c4e78b"]
          },
          "legend": {"orient": "bottom", "direction": "horizontal", "columns": 4}
        },
        "opacity": {
          "condition": {"param": "driverHighlight", "value": 0.9},
          "value": 0.25
        },
        "tooltip": [
          {"field": "Year"},
          {"field": "Driver"},
          {"field": "Index", "format": ".2f"}
        ]
      }
    },
    {
      "mark": {"type": "line", "strokeWidth": 3},
      "encoding": {
        "x": {"field": "Year", "type": "ordinal"},
        "y": {
          "field": "RainfallAnomaly",
          "type": "quantitative",
          "axis": {"title": "Rainfall Anomaly (mm)", "orient": "right"}
        },
        "color": {
          "datum": "Rainfall Anomaly",
          "type": "nominal",
          "scale": {"range": ["#a40000ff"]},
          "legend": {"title": null, "orient": "bottom", "direction": "horizontal", "columns": 4}
        },
        "tooltip": [
          {"field": "Year"},
          {"field": "RainfallAnomaly", "title": "Rainfall (mm)", "format": ".1f"}
        ]
      }
    },
    {
      "layer": [
        {
          "mark": {
            "type": "rule",
            "stroke": "#333333",
            "strokeWidth": 1,
            "strokeDash": [5, 5]
          },
          "encoding": {
            "x": {"value": 140},
            "y": {"value": 20},
            "x2": {"value": 150},
            "y2": {"value": 15}
          }
        },
        {
          "mark": {
            "type": "text",
            "align": "center",
            "baseline": "top",
            "dy": -90,
            "dx": 10,
            "fontSize": 9,
            "fontStyle": "lato",
            "lineBreak": "\n",
            "lineHeight": 14
          },
          "encoding": {
            "x": {"value":190},
            "y": {"value":100},
            "text": {"value": "Major flood Due to \nLa Nina & Negative IOD "},
            "color": {"value": "#333333"}
          }
        }
        ]
    },
       {
      "layer": [
        {
          "mark": {
            "type": "rule",
            "stroke": "#333333",
            "strokeWidth": 1,
            "strokeDash": [5, 5]
          },
          "encoding": {
            "x": {"value": 440},
            "y": {"value": 185},
            "x2": {"value": 400},
            "y2": {"value": 170}
          }
        },
        {
          "mark": {
            "type": "text",
            "align": "center",
            "baseline": "top",
            "dy": -90,
            "dx": 10,
            "fontSize": 9,
            "fontStyle": "lato",
            "lineBreak": "\n",
            "lineHeight": 14
          },
          "encoding": {
            "x": {"value":340},
            "y": {"value":250},
            "text": {"value": "Major Fires Due to \nPositive IOD & El Nino "},
            "color": {"value": "#333333"}
          }
        }
        ]
    }
    
  ],

  "resolve": {"scale": {"y": "independent", "color": "independent"}},

  "config": {
    "background": "transparent",
    "axis": {"labelFont": "Lato", "titleFont": "Lato"},
    "legend": {
      "labelFont": "Lato",
      "titleFont": "Lato",
      "orient": "bottom",
      "direction": "horizontal",
      "symbolSize": 150,
      "padding": 10,
      "gradientLength": 300,
      "gradientThickness": 14,
      "titleFontSize": 13,
      "labelFontSize": 12
    }
  }
},embedOpt);

vegaEmbed("#section3-viz", {
  "$schema": "https://vega.github.io/schema/vega/v5.json",
  "description": "A compact radar chart showing climate metrics for Australia and Global.",
  "width": 400,          // Increased from 300
  "height": 300,         // Increased from 250
  "padding": {"left": 80, "top": 50, "bottom": 10, "right": 45}, // Adjusted padding
  "autosize": {"type": "none", "contains": "padding"},
  "background": "transparent",

  "signals": [
    {"name": "radius", "update": "min(width, height) / 2.3"} // Increased from / 3.5
  ],

  "data": [
    {
      "name": "table",
      "url": "https://raw.githubusercontent.com/alie0025/DV2_Final/refs/heads/main/radar_chart.csv",
      "format": {"type": "csv", "parse": {"Value": "number"}},
      "transform": [
        {"type": "formula", "as": "category", "expr": "datum.Country"}
      ]
    },
    {
      "name": "keys",
      "source": "table",
      "transform": [{"type": "aggregate", "groupby": ["Metric"]}]
    },
    {
      "name": "legend_data",
      "values": [
        {"label": "Australia", "color": "#1f77b4"},
        {"label": "Global", "color": "#c092f1ff"}
      ]
    }
  ],

  "scales": [
    {
      "name": "angular",
      "type": "point",
      "range": {"signal": "[-PI, PI]"},
      "padding": 0.5,
      "domain": {"data": "table", "field": "Metric"}
    },
    {
      "name": "radial",
      "type": "linear",
      "range": {"signal": "[0, radius]"},
      "zero": true,
      "nice": false,
      "domain": {"data": "table", "field": "Value"},
      "domainMin": 0,
      "domainMax": 1
    },
    {
      "name": "color",
      "type": "ordinal",
      "domain": {"data": "table", "field": "category"},
      "range": ["#1f77b4", "#c092f1ff"]
    }
  ],

  "encode": {
    "enter": {"x": {"signal": "radius"}, "y": {"signal": "radius"}}
  },

  "marks": [
    {
      "type": "group",
      "name": "categories",
      "zindex": 1,
      "from": {"facet": {"data": "table", "name": "facet", "groupby": ["category"]}},
      "marks": [
        {
          "type": "line",
          "name": "category-line",
          "from": {"data": "facet"},
          "encode": {
            "enter": {
              "interpolate": {"value": "linear-closed"},
              "x": {"signal": "scale('radial', datum.Value) * cos(scale('angular', datum.Metric))"},
              "y": {"signal": "scale('radial', datum.Value) * sin(scale('angular', datum.Metric))"},
              "stroke": {"scale": "color", "field": "category"},
              "strokeWidth": {"value": 1.5},
              "fill": {"scale": "color", "field": "category"},
              "fillOpacity": {"value": 0.1},
              "tooltip": {"signal": "{'Metric': datum.Metric, 'Value': format(datum.Value, '.2f'), 'Country': replace(datum.category, '_scaled', '')}"}
            }
          }
        }
      ]
    },
    {
      "type": "rule",
      "name": "radial-grid",
      "from": {"data": "keys"},
      "zindex": 0,
      "encode": {
        "enter": {
          "x": {"value": 0},
          "y": {"value": 0},
          "x2": {"signal": "radius * cos(scale('angular', datum.Metric))"},
          "y2": {"signal": "radius * sin(scale('angular', datum.Metric))"},
          "stroke": {"value": "#d3d3d3"},
          "strokeWidth": {"value": 0.8}
        }
      }
    },
    {
      "type": "text",
      "name": "key-label",
      "from": {"data": "keys"},
      "zindex": 1,
      "encode": {
        "enter": {
          "x": {"signal": "(radius + 12) * cos(scale('angular', datum.Metric))"}, // Increased offset
          "y": {"signal": "(radius + 12) * sin(scale('angular', datum.Metric))"}, // Increased offset
          "text": {"field": "Metric"},
          "align": [
            {"test": "abs(scale('angular', datum.Metric)) > PI / 2", "value": "right"},
            {"value": "left"}
          ],
          "baseline": [
            {"test": "scale('angular', datum.Metric) > 0", "value": "top"},
            {"test": "scale('angular', datum.Metric) == 0", "value": "middle"},
            {"value": "bottom"}
          ],
          "fill": {"value": "#333"},
          "fontSize": {"value": 9},  // Kept at 9, adjustable if needed
          "fontWeight": {"value": "bold"}
        }
      }
    },
    {
      "type": "line",
      "name": "outer-line",
      "from": {"data": "radial-grid"},
      "encode": {
        "enter": {
          "interpolate": {"value": "linear-closed"},
          "x": {"field": "x2"},
          "y": {"field": "y2"},
          "stroke": {"value": "#d3d3d3"},
          "strokeWidth": {"value": 1.5}
        }
      }
    },
    {
  "type": "group",
  "encode": {
    "enter": {
      "x": {"signal": "width/2 - 50"},   // Center horizontally
      "y": {"signal": "height - 40"}     // Below chart
    }
  },
  "marks": [
    {
      "type": "rect",
      "from": {"data": "legend_data"},
      "encode": {
        "enter": {
          "x": {"signal": "datum.label === 'Global' ? 80 : 0"},  // second item shifted right
          "y": {"value": 0},
          "width": {"value": 16},
          "height": {"value": 16},
          "fill": {"field": "color"},
          "stroke": {"value": "#333"},
          "strokeWidth": {"value": 1}
        }
      }
    },
    {
      "type": "text",
      "from": {"data": "legend_data"},
      "encode": {
        "enter": {
          "x": {"signal": "datum.label === 'Global' ? 102 : 22"}, // align text beside each box
          "y": {"value": 12},
          "text": {"field": "label"},
          "fontSize": {"value": 12},
          "fill": {"value": "#333"},
          "align": {"value": "left"}
        }
      }
    }
  ]
}
  ]
}, embedOpt);


vegaEmbed("#section4-viz", {
  "$schema": "https://vega.github.io/schema/vega-lite/v5.json",
  "data": {"url": "https://raw.githubusercontent.com/alie0025/DV2_Final/refs/heads/main/bubble_plot_dataset_casualties.csv"},
  "width": 580,
  "height": 300,
  "background": "transparent",
  "params": [
    {
      "name": "yearSelector",
      "value": 2025,
      "bind": {
        "input": "range",
        "min": 2020,
        "max": 2025,
        "step": 1,
        "name": "Select Year:"
      }
    },
    {
      "name": "eventTypeFilter",
      "value": "Wildfire",
      "bind": {
        "input": "select",
        "options": [
          "Drought",
          "Earthquake",
          "Flood",
          "Hailstorm",
          "Heatwave",
          "Landslide",
          "Wildfire",
          "Cold Wave",
          "Hurricane",
          "Tornado",
          "Tsunami",
          "Volcanic Eruption"
        ],
        "name": "Select Event Type:"
      }
    }
  ],

  "transform": [
    {"calculate": "toNumber(datum.year)", "as": "year_num"},
    {"filter": "datum.year_num == yearSelector"},
    {"filter": "datum.event_type == eventTypeFilter"}
  ],

  "layer": [
    {
      "params": [
        {
          "name": "continentHighlight",
          "select": {"type": "point", "fields": ["continent"]},
          "bind": "legend"
        }
      ],
      "mark": {"type": "circle", "opacity": 0.7},
      "encoding": {
        "x": {
          "field": "avg_severity",
          "type": "quantitative",
          "axis": {"title": "Average Severity (1–10)","grid": false },
          "scale": {"domain": [0, 8]}
        },
        "y": {
          "field": "total_economic_impact_million_usd",
          "type": "quantitative",
          "axis": {"title": "Total Economic Impact (Million USD)","grid": false },
          "scale": {"zero": false, "nice": true}
        },
        "size": {
          "field": "total_casualties",
          "type": "quantitative",
          "scale": {"range": [100, 800]},
          "legend": {"title": "Total Casualties"}
        },
        "color": {
          "field": "continent",
          "type": "nominal",
          "legend": {"title": "Continent"},
          "scale": {
            "domain": ["Asia","Europe","Africa","North America","South America","Oceania"],
            "range": ["#e41a1c","#a65628","#ff7f00","#ecef5cff","#984ea3","#377eb8"]
          }
        },
        "opacity": {
  "condition": {"param": "continentHighlight", "value": 0.9},
  "value": 0.15
},
        "tooltip": [
          {"field": "country", "title": "Country"},
          {"field": "event_type", "title": "Event Type"},
          {"field": "year", "title": "Year"},
          {
            "field": "avg_severity",
            "title": "Avg Severity (1–10)",
            "type": "quantitative",
            "format": ".1f"
          },
          {
            "field": "total_casualties",
            "title": "Total Casualties",
            "type": "quantitative",
            "format": ","
          },
          {
            "field": "total_economic_impact_million_usd",
            "title": "Impact (Million USD)",
            "type": "quantitative",
            "format": ",.2f"
          },
          {
            "field": "n_events",
            "title": "Event Count",
            "type": "quantitative",
            "format": ","
          }
        ]
      }
    },
    // {
    //   "transform": [
    //     {"window": [{"op": "rank", "as": "impactRank"}], "sort": [{"field": "total_economic_impact_million_usd", "order": "descending"}]},
    //     {"filter": "datum.impactRank <= 5"}
    //   ],
    //   "mark": {"type": "text", "align": "right", "baseline": "middle", "dx": -10, "fontSize": 12, "fontStyle": "lato"},
    //   "encoding": {
    //     "x": {"field": "avg_severity", "type": "quantitative"},
    //     "y": {"field": "total_economic_impact_million_usd", "type": "quantitative"},
    //     "text": {"field": "country", "type": "nominal"},
    //     "color": {"value": "black"}
    //   }
    // },
{
      "transform": [
        {"filter": "datum.country == 'Australia' && datum.event_type == 'Wildfire' && datum.year == 2025"},
        {"window": [{"op": "row_number", "as": "rank"}], "sort": [{"field": "total_economic_impact_million_usd", "order": "descending"}]},
        {"filter": "datum.rank == 1"} // Annotate the top wildfire event in Australia
      ],
      "layer": [
        {
          "mark": {
            "type": "rule",
            "stroke": "#333333",
            "strokeWidth": 1,
            "strokeDash": [5, 5] // Dashed line for visual distinction
          },
          "encoding": {
          "x": {"value": 435}, // Hardcoded start x (e.g., approx. severity)
          "y": {"value": 270}, // Hardcoded start y (e.g., approx. impact in million USD)
          "x2": {"value": 430}, // Hardcoded end x (same as start for vertical line)
          "y2": {"value":240}
          }
        },
        {
          "mark": {
            "type": "text",
            "align": "center",
            "baseline": "top",
            "dy": -90, // Increased space from -5 to -15
            "dx": 50,
            "fontSize": 8,
            "fontStyle": "lato",
            "lineBreak": "\n",      // ← enables \n line breaks
             "lineHeight": 14 
          },
          "encoding": {
            "x": {"field": "avg_severity", "type": "quantitative"},
            "y": {"field": "total_economic_impact_million_usd", "type": "quantitative"},
            "text": {"value": "Major Wildfire Event, Australia (2025)\nOccurred Oct 2025, devastated SE states\nAffected 1,500 people, cost 120M USD\nAcross 15 events, triggered nat'l emergency"},
            "color": {"value": "#333333"} // Dark gray for contrast
          }
        }
      ]
    },

    {
      "transform": [
        {"filter": "datum.country == 'Italy' && datum.event_type == 'Wildfire' && datum.year == 2025"},
        {"window": [{"op": "row_number", "as": "rank"}], "sort": [{"field": "total_economic_impact_million_usd", "order": "descending"}]},
        {"filter": "datum.rank == 1"} // Annotate the top wildfire event in Italy
      ],
      "layer": [
        {
          "mark": {
            "type": "rule",
            "stroke": "#333333",
            "strokeWidth": 1,
            "strokeDash": [5, 5] // Dashed line for visual distinction
          },
          "encoding": {
            "x": {"value": 330}, // Hardcoded start x (e.g., approx. severity)
            "y": {"value": 160}, // Hardcoded start y (e.g., approx. impact in million USD)
            "x2": {"value": 200}, // Hardcoded end x (adjusted for text offset)
            "y2": {"value": 125}  // Hardcoded end y (start y - 20)
          }
        },
        {
          "mark": {
            "type": "text",
            "align": "center",
            "baseline": "top",
            "dy": -90, // Increased space for multi-line text
            "dx":-200,
            "fontSize": 8,
            "fontStyle": "lato",
            "lineBreak": "\n",      // ← enables \n line breaks
            "lineHeight": 14 
          },
          "encoding": {
            "x": {"field": "avg_severity", "type": "quantitative"},
            "y": {"field": "total_economic_impact_million_usd", "type": "quantitative"},
            "text": {"value": "Major Wildfire Event, Italy (2025)\nOccurred Oct 2025, devastated S regions\nAffected 800 people, cost 80M USD\nAcross 10 events, triggered nat'l response"},
            "color": {"value": "#333333"} // Dark gray for contrast
          }
        }
      ]
    },
    {
      "transform": [
        {"filter": "datum.country == 'Australia' && datum.event_type == 'Flood' && datum.year == 2023"},
        {"window": [{"op": "row_number", "as": "rank"}], "sort": [{"field": "total_economic_impact_million_usd", "order": "descending"}]},
        {"filter": "datum.rank == 1"} // Annotate the top flood event in Australia 2023
      ],
      "layer": [
        {
          "mark": {
            "type": "rule",
            "stroke": "#333333",
            "strokeWidth": 1,
            "strokeDash": [5, 5] // Dashed line for visual distinction
          },
          "encoding": {
            "x": {"value": 470}, // Hardcoded start x (e.g., approx. severity for flood)
            "y": {"value": 270}, // Hardcoded start y (e.g., approx. impact ~650M USD)
            "x2": {"value": 440}, // Hardcoded end x (adjusted for text offset)
            "y2": {"value": 230} // Hardcoded end y (start y - 30)
          }
        },
        {
          "mark": {
            "type": "text",
            "align": "center",
            "baseline": "top",
            "dy": -120, // Increased space for multi-line text
            "dx": -50,
            "fontSize": 8,
            "fontStyle": "lato",
            "lineBreak": "\n",      // ← enables \n line breaks
            "lineHeight": 14 
          },
          "encoding": {
            "x": {"field": "avg_severity", "type": "quantitative"},
            "y": {"field": "total_economic_impact_million_usd", "type": "quantitative"},
            "text": {"value": "Major Flood Event, Australia (2023)\nOccurred December 2023, post-Cyclone Jasper\nDevastated Far North Queensland (Cairns)\nAffected ~1,000 people, cost ~650M USD\nAcross 10 events, major evacuations & airport closures"},
            "color": {"value": "#333333"} // Dark gray for contrast
          }
        }
      ]
    }
    
  ],

"config": {
  "background": "transparent",
  "axis": {
    "labelColor": "#333333",
    "titleColor": "#333333"
  },
  "legend": {
    "labelFontSize": 12,
    "titleFontSize": 13,
    "labelColor": "#333333",
    "titleColor": "#333333"
  },
   "style": {
    "guide-label": {"font": "Lato"},
    "tooltip": {
      "content": "encoding",
      "labelFont": "Lato",
      "labelFontSize": 13,
      "titleFont": "Lato",
      "titleFontWeight": "normal",
      "titleFontSize": 13
    }
  }
}
},embedOpt);




vegaEmbed("#section5-viz", {
  "$schema": "https://vega.github.io/schema/vega/v5.json",
  "width": 350,
  "height": 250,
  "autosize": "pad",
  "background": "transparent",

  "signals": [
    {
      "name": "yearPicked",
      "value": 2020,
      "bind": {
        "input": "range",
        "min": 2020,
        "max": 2025,
        "step": 1,
        "name": "Year → "
      }
    },
    {
      "name": "hover",
      "value": null,
      "on": [
        {"events": "rect:mouseover", "update": "datum"},
        {"events": "rect:mouseout", "update": "null"}
      ]
    }
  ],

  "data": [
    {
      "name": "ausDisasters",
      "url": "https://raw.githubusercontent.com/alie0025/DV2_Final/refs/heads/main/bubble_plot_dataset_casualties.csv",
      "format": {"type": "csv"},
      "transform": [
        {"type": "filter", "expr": "datum.country == 'Australia' && datum.year == yearPicked"},
        {"type": "nest", "keys": ["event_type"]},
        {
          "type": "treemap",
          "field": "n_events",
          "method": "squarify",
          "ratio": 1.4,
          "round": true,
          "paddingInner": 2,
          "size": [{"signal": "width"}, {"signal": "height"}]
        }
      ]
    }
  ],

  "scales": [
    {
      "name": "clr",
      "type": "ordinal",
      "domain": [
        "Drought","Earthquake","Flood","Hailstorm","Heatwave","Landslide",
        "Wildfire","Cold Wave","Hurricane","Tornado","Tsunami","Volcanic Eruption"
      ],
      "range": [
   "#0072B2", // Blue
  "#E69F00", // Orange
  "#56B4E9", // Sky Blue
  "#CC79A7", // Magenta
  "#D55E00", // Vermilion
  "#F0E442", // Yellow
  "#f97d7dff", // Grey
  "#A6761D", // Brown 
  "#7570B3", // Violet
  "#66A61E", // Olive Green (muted)
  "#8DD3C7", // Aqua / Mint
  "#B3DE69"  // Light Olive-Yellow
      ]
    }
  ],

  "marks": [
    {
      "name": "cells",
      "type": "rect",
      "from": {"data": "ausDisasters"},
      "encode": {
        "enter": {
          "stroke": {"value": "#fff"},
          "strokeWidth": {"value": 1.5},
          "cornerRadius": {"value": 4}
        },
        "update": {
          "x": {"field": "x0"},
          "x2": {"field": "x1"},
          "y": {"field": "y0"},
          "y2": {"field": "y1"},
          "fill": {"scale": "clr", "field": "event_type"},
          "strokeWidth": [
            {"test": "hover && hover.event_type === datum.event_type", "value": 3},
            {"value": 1.5}
          ],
          "tooltip": {
            "signal": "{'Type': datum.event_type, 'Events': datum.n_events, " +
                      "'Severity': datum.avg_severity, 'Casualties': datum.total_casualties, " +
                      "'Impact (M USD)': format(datum.total_economic_impact_million_usd, '.2f'), 'Year': datum.year}"
          }
        }
      }
    },

    {
      "type": "text",
      "from": {"data": "ausDisasters"},
      "encode": {
        "enter": {
          "fontSize": {"value": 12},
          "fontWeight": {"value": "bold"},
          "fill": {"value": "#111"}
        },
        "update": {
          "x": {"signal": "datum.x0 + 5"},
          "y": {"signal": "datum.y0 + 16"},
          "text": {"field": "event_type"},
          "opacity": [
            {"test": "hover && hover.event_type === datum.event_type", "value": 1},
            {"value": 0.9}
          ]
        }
      }
    }
  ],
  "config": {
    "title": {"font": "Lato"},
    "style": {"cell": {"stroke": "transparent"}}
  }
}, embedOpt);





const koalaMapSpec = {
  "$schema": "https://vega.github.io/schema/vega-lite/v5.json",
  "background": "transparent",
  "title": {
    "anchor": "start",
    "fontSize": 18,
    "fontWeight": "bold",
    "offset": 10
  },
  "width": 780,    // was 900
  "height": 480,   // was 500

  "projection": {
    "type": "albers",
    "rotate": [-132, 0],
    "translate": [330, 250],  
    "center": [0, -27],
    "parallels": [-18, -36],
    "scale": 780   // was 850
  },

  "layer": [
    {
  "data": {
    "url": "https://raw.githubusercontent.com/alie0025/DV2_Final/refs/heads/main/ne_10m_admin_1_states_provinces.json",
    "format": {"type": "topojson", "feature": "ne_10m_admin_1_states_provinces"}
  },
  "transform": [{"filter": "datum.properties.admin == 'Australia'"}],
  "mark": {"type": "geoshape", "stroke": "#777", "fill": "#e6ebf4", "strokeWidth": 0.8}
},
{
  "data": {
    "url": "https://raw.githubusercontent.com/alie0025/DV2_Final/refs/heads/main/ne_10m_admin_1_states_provinces.json",
    "format": {"type": "topojson", "feature": "ne_10m_admin_1_states_provinces"}
  },
  "transform": [{"filter": "datum.properties.admin == 'Australia'"}],
  "mark": {"type": "text", "fontSize": 9, "fontWeight": "bold"},
  "encoding": {
    "longitude": {"field": "properties.longitude"},
    "latitude": {"field": "properties.latitude"},
    "text": {"field": "properties.postal"},
    "color": {"value": "#333"}
  }
},
// {
//   "data": {
//     "url": "https://raw.githubusercontent.com/alie0025/DV2_Final/refs/heads/main/ne_10m_admin_1_states_provinces.json",
//     "format": {"type": "topojson", "feature": "ne_10m_admin_1_states_provinces"}
//   },
//   "transform": [{"filter": "datum.properties.admin == 'Australia'"}],
//   "mark": {"type": "geoshape", "stroke": "#777", "fill": "#e6ebf4", "strokeWidth": 0.8},
//   "encoding": {
//     "text": {"field": "properties.postal"}
//   }
// },

//  {
//       "data": {
//         "url": "https://raw.githubusercontent.com/alie0025/DV2_Final/refs/heads/main/ne_10m_admin_1_states_provinces.json",
//         "format": {"type": "topojson", "feature": "ne_10m_admin_1_states_provinces"}
//       },
//       "transform": [
//         {"filter": "datum.properties['name'] != 'Australian Capital Territory'"}
//       ],
//       "mark": {"type": "text", "fontSize": 13},
//       "encoding": {
//         "longitude": {"field": "properties.longitude", "type": "quantitative"},
//         "latitude": {"field": "properties.latitude", "type": "quantitative"},
//         "text": {"field": "properties.name", "type": "nominal"},
//         "color": {"value": "black"}
//       }
//     },
  {
    "data": {
      "url": "https://raw.githubusercontent.com/alie0025/DV2_Final/refs/heads/main/koala_habitat_core.json",
      "format": {"type": "topojson", "feature": "koala_habitat_core"}
    },
    "mark": {"type": "geoshape"},
    "encoding": {
      "color": {"value": "#17801cff"},
      "opacity": {"value": 0.5},
       "legend": {
        "title": null,
        "orient": "right",
        "offset": -120,          // move legend closer to the map
        "labelFont": "Lato",
        "labelFontSize": 12,
        "symbolSize": 150
      }
    }
  },

     /* --- Bushfire hotspot points --- */
    {
      "data": {
        "url": "https://raw.githubusercontent.com/alie0025/DV2_Final/refs/heads/main/fire_dataset.csv",
        "format": {"type": "csv"}
      },
      "mark": {"type": "circle", "opacity": 0.8, "size": 10},
      "encoding": {
        "longitude": {"field": "longitude", "type": "quantitative"},
        "latitude": {"field": "latitude", "type": "quantitative"},
        "color": {
          "field": "brightness",
          "type": "quantitative",
          "title": "Brightness (K)",
                "scale": {
        "domain": [280, 370],       // tighter range
        "range": ["#ffcccc", "#610000ff"]      // same colour scheme
          },
        "legend": {
        "orient": "right",
        "offset": -130,
        "padding": 0,
        "titleFontSize": 12,
        "labelFontSize": 11
    },
        },
        "tooltip": [
          {"field": "latitude", "title": "Latitude"},
          {"field": "longitude", "title": "Longitude"},
          {"field": "brightness", "title": "Brightness (K)"}
        ]
      }
    }
  ],
  "config": {
    "background": "transparent",
    "style": {
      "guide-label": {"font": "Lato"},
      "tooltip": {
        "content": "encoding",
        "labelFont": "Lato",
        "labelFontSize": 13,
        "titleFont": "Lato",
        "titleFontSize": 13
      }
    }
  }

};

vegaEmbed("#section6-map", koalaMapSpec, embedOpt);



vegaEmbed("#section7-viz", {
  "$schema": "https://vega.github.io/schema/vega-lite/v5.json",
  "description": "Two separate bar charts for Endangered and Vulnerable species, filterable by taxonomic group.",
  "data": {"url": "https://raw.githubusercontent.com/alie0025/DV2_Final/refs/heads/main/filtered_fire_impact3.csv"},
  "height": 450,
   "autosize": {"type": "fit", "contains": "padding"},
  "background": "transparent",

  "params": [
    {
      "name": "groupSelector",
      "value": "Mammalia",
      "bind": {
        "input": "select",
        "options": ["Mammalia", "Aves", "Reptilia", "Amphibia"],
        "name": "Select Taxonomic Group: "
      }
    }
  ],

  "hconcat": [
    {
      "height": 300,
      "width": 150, 
      "transform": [
        {"filter": "datum['Taxonomic group'] == groupSelector"},
        {"filter": "datum['Threat status'] == 'Endangered'"},
        {"filter": "isValid(datum['Proportional burned (%)'])"}
      ],
      "mark": {
        "type": "bar",
        "cornerRadiusTopLeft": 3,
        "cornerRadiusTopRight": 3,
        "tooltip": true
      },
      "encoding": {
        "y": {
          "field": "Scientific name",
          "type": "nominal",
          "sort": "-x",
          "axis": {"title": "Endangered Species", "labelLimit": 180}
        },
        "x": {
          "field": "Proportional burned (%)",
          "type": "quantitative",
          "axis": {"title": "Proportional Burned (%)"},
          "scale": {"domain": [0, 100]}
        },
        "color": {
          "field": "Taxonomic group",
          "type": "nominal",
          "legend": null,
          "scale": {
            "domain": ["Mammalia", "Aves", "Reptilia", "Amphibia"],
            "range": ["#4e79a7", "#f46d43", "#9467bd", "#1a9850"]
          }
        },
        "tooltip": [
          {"field": "Scientific name", "title": "Species"},
          {"field": "Threat status", "title": "Threat Status"},
          {"field": "Taxonomic group", "title": "Taxonomic Group"},
          {"field": "Total habitat (Ha)", "title": "Total Habitat (Ha)", "format": ","},
          {"field": "Burned habitat (Ha)", "title": "Burned Habitat (Ha)", "format": ","},
          {"field": "Proportional burned (%)", "title": "% Burned"},
          {"field": "Unburned habitat (Ha)", "title": "Unburned (Ha)", "format": ","}
        ]
      }
    },
    {
      "height": 300,
      "width" : 150,
      "transform": [
        {"filter": "datum['Taxonomic group'] == groupSelector"},
        {"filter": "datum['Threat status'] == 'Vulnerable'"},
        {"filter": "isValid(datum['Proportional burned (%)'])"}
      ],
      "mark": {
        "type": "bar",
        "cornerRadiusTopLeft": 3,
        "cornerRadiusTopRight": 3,
        "tooltip": true
      },
      "encoding": {
        "y": {
          "field": "Scientific name",
          "type": "nominal",
          "sort": "-x",
          "axis": {"title": "Vulnerable Species", "labelLimit": 180}
        },
        "x": {
          "field": "Proportional burned (%)",
          "type": "quantitative",
          "axis": {"title": "Proportional Burned (%)"},
          "scale": {"domain": [0, 100]}
        },
        "color": {
          "field": "Taxonomic group",
          "type": "nominal",
          "legend": null,
          "scale": {
            "domain": ["Mammalia", "Aves", "Reptilia", "Amphibia"],
            "range": ["#4e79a7", "#f46d43", "#9467bd", "#1a9850"]
          }
        },
        "tooltip": [
          {"field": "Scientific name", "title": "Species"},
          {"field": "Threat status", "title": "Threat Status"},
          {"field": "Taxonomic group", "title": "Taxonomic Group"},
          {"field": "Total habitat (Ha)", "title": "Total Habitat (Ha)", "format": ","},
          {"field": "Burned habitat (Ha)", "title": "Burned Habitat (Ha)", "format": ","},
          {"field": "Proportional burned (%)", "title": "% Burned"},
          {"field": "Unburned habitat (Ha)", "title": "Unburned (Ha)", "format": ","}
        ]
      }
    }
  ],

  "resolve": {"scale": {"y": "independent"}},
  "config": {
    "axis": {"labelColor": "#333", "titleColor": "#333"},
    "legend": {
      "labelFontSize": 12,
      "titleFontSize": 13,
      "labelColor": "#333",
      "titleColor": "#333"
    },
    "background": "transparent",
    "view": {
      "stroke": null // Remove borders between charts
    },
    "style": {
      "guide-label": {"font": "Lato"},
      "tooltip": {
        "content": "encoding",
        "labelFont": "Lato",
        "labelFontSize": 13,
        "titleFont": "Lato",
        "titleFontWeight": "normal",
        "titleFontSize": 13
      }
    }
  }
}, embedOpt);
