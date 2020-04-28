function setupDraggable(sendEvent) {
    const BEACON_ATTRIBUTE = "data-beacon";
    const MINIMUM_DRAG_DISTANCE_PX = 10;

    function dragEvent(type, event) {
        sendEvent({
            type: type,
            cursor: coords(event),
            beacons: beaconPositions()
        });
    }

    function dragEnd(event) {
        dragEvent("stop", event);

        document.removeEventListener("pointermove", dragMove);
        document.removeEventListener("pointerup", dragEnd);
    }

    function dragMove(event) {
        dragEvent("move", event);
    }

    function beaconPositions() {
        const beaconElements = document.querySelectorAll(`[${BEACON_ATTRIBUTE}]`);
        return Array.from(beaconElements).map(beaconData);
    }

    function beaconData(elem) {
        const boundingRect = elem.getBoundingClientRect();
        const beaconId = elem.getAttribute(BEACON_ATTRIBUTE);

        return {
            id: beaconId,
            x: boundingRect.x,
            y: boundingRect.y,
        };
    }

    function coords(event) {
        return { x: event.clientX, y: event.clientY };
    }

    function distance(pos1, pos2) {
        const dx = pos1.x - pos2.x;
        const dy = pos1.y - pos2.y;

        return Math.sqrt(Math.pow(dx, 2) + Math.pow(dy, 2));
    }

    function awaitDragStart(startEvent) {
        document.addEventListener("pointermove", maybeDragMove);
        document.addEventListener("pointerup", stopAwaitingDrag);

        function stopAwaitingDrag() {
            document.removeEventListener("pointermove", maybeDragMove);
            document.removeEventListener("pointerup", stopAwaitingDrag);
        }

        // prevent clicks from being a drag
        function maybeDragMove(moveEvent) {
            const dragDistance = distance(coords(startEvent), coords(moveEvent));
            console.log(dragDistance)

            if (dragDistance >= MINIMUM_DRAG_DISTANCE_PX) {
                dragEvent("start", startEvent);
                dragEvent("move", moveEvent);
                stopAwaitingDrag();
                document.addEventListener("pointermove", dragMove);
                document.addEventListener("pointerup", dragEnd);
            }
        }
    }

    document.addEventListener("pointerdown", awaitDragStart);
};
