action(
    set_position(PositionNew),
    [position_set(PositionOld)],
    [position_set(PositionNew)],
    [position_set(PositionOld)],
    [pos(PositionNew)],
    [
        del(position_set(PositionOld)), 
        add(position_set(PositionNew))
    ]
).

action(
    high_level_scan_start(Agent), 
    [available(Agent), position_set(supine)], 
    [high_level_scanned], 
    [],
    [video(Agent)], 
    [
        del(available(Agent)), 
        add(high_level_scanning(Agent))
    ]
).

action(
    high_level_scan_end(Agent), 
    [high_level_scanning(Agent), position_set(supine)], 
    [],
    [], 
    [],
    [
        del(high_level_scanning(Agent)),
        add(available(Agent)), add(high_level_scanned)
    ]
).

action(
    model_mapping(Model),
    [high_level_scanned],
    [model_set(_)],
    [],
    [mdl(Model)],
    [
        add(model_set(Model))
    ]
).

action(
    find_body_segment(blueP(Point)), 
    [high_level_scanned, model_set(_), toScan(Point)], 
    [identified_area(Point), marked(Point), scanned(Point)], 
    [], 
    [bP(Point)], 
    [
        del(toScan(Point)),
        add(identified_area(Point))
    ]
).

action(
    detailed_video_scan_start(Agent, Point), 
    [available(Agent), identified_area(Point)], 
    [detailed_video_scanned(_, Point), detailed_video_scanning(_, Point), palpating(_, Point), scanned(Point)], 
    [], 
    [bP(Point), video(Agent)], 
    [
        del(available(Agent)),
        add(detailed_video_scanning(Agent, Point))
    ]
).

action(
    detailed_video_scan_end(Agent, Point), 
    [detailed_video_scanning(Agent, Point)], 
    [], 
    [], 
    [], 
    [
        del(detailed_video_scanning(Agent, Point)),
        add(detailed_video_scanned(Agent, Point)), add(available(Agent))
    ]
).

action(
    palpation_start(Agent, Point), 
    [available(Agent), identified_area(Point)], 
    [detailed_video_scanning(_, Point), palpated(_, Point), palpating(_, Point), scanned(Point)], 
    [], 
    [bP(Point), palpation(Agent)], 
    [
        del(available(Agent)),
        add(palpating(Agent, Point))
    ]
).

action(
    palpation_end(Agent, Point), 
    [palpating(Agent, Point)], 
    [],
    [], 
    [], 
    [
        del(palpating(Agent, Point)),
        add(available(Agent)), add(palpated(Agent, Point))
    ]
).

action(
    sensor_fusion(Point), 
    [palpated(Agent, Point), detailed_video_scanned(Agent, Point), identified_area(Point)], 
    [marked(Point), scanned(Point)],
    [scanned(Point)], 
    [bP(Point)], 
    [
        del(palpating(Agent, Point)), del(detailed_video_scanned(Agent, Point)), del(identified_area(Point)),
        add(marked(Point))
    ]
).

action(
    gel_application_start(Agent, Point),
    [marked(Point), available(Agent)],
    [gel_appling(_, _), gel_applied(_, Point), scanned(Point)],
    [scanned(Point)],
    [gel_applier(Agent)],
    [
        del(available(Agent)),
        add(gel_appling(Agent, Point))
    ]
).

action(
    gel_application_end(Agent, Point),
    [gel_appling(Agent, Point)],
    [],
    [],
    [],
    [
        del(gel_appling(Agent, Point)),
        add(available(Agent)), add(gel_applied(Point))
    ]
).

action(
    scan_start(Agent, Point),
    [gel_applied(Point), available(Agent)],
    [scanning(_, Point), scanned(Point)],
    [scanned(Point)],
    [scanner(Agent)],
    [
        del(available(Agent)),
        add(scanning(Agent, Point))
    ]
).

action(
    scan_end(Agent, Point),
    [scanning(Agent, Point)],
    [],
    [],
    [],
    [
        del(scanning(Agent, Point)), del(gel_applied(Point)), del(marked(Point)),
        add(available(Agent)), add(scanned(Point))
    ]
).
