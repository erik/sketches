/* jshint esversion: 6 */

const map = L.map('map').setView([34, -118], 10);

L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png').addTo(map);

fetch('/where')
    .then(data => data.json())
    .then(json => json.where)
    .then(points => {
        if (points.length === 0) return;

        const polyline = L.polyline(points.map(it => [it.lat, it.lng])).addTo(map);
        map.fitBounds(polyline.getBounds());

        points.forEach(point => {
            L.marker([point.lat, point.lng])
                .bindPopup(`<p>${point.comment}</p><p>${point.ts}</p>`)
                .addTo(map);
        });
    })
    .catch(err => { alert(`Failed to get coordinates: ${err}`); });
