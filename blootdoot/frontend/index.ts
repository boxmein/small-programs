
console.log("hello");

function startbt() {
    navigator.bluetooth.requestDevice({
        filters: [
            {
                services: [
                    'battery_service'
                ]
            }
        ]
    }).then((device) => {
        console.log('Received device', device);
    }).catch((err) => console.error(err));
}

document.getElementById('connect').addEventListener('click', startbt);
