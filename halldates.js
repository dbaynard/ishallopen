var then = moment("2016-06-25");
var now = moment();
var today = now.diff(then, 'days');

function getMessage(day) {
    if (day === 17) {
        return 'There’s an MCR dinner tonight!'
    } else if (day >= 96 || day < 0) {
        return 'Who knows. Better get on your bike.'
    } else if (day < 48 || day >= 67) {
        switch (day % 7) {
            case 0:
                return 'Hall is closed today.'
                break;
            case 1:
                return 'Hall is serving dinner today.'
                break;
            default:
                return 'Hall is serving breakfast and lunch today.';
        }
    } else if (day >= 48 && day < 68) {
        return 'Hall is closed until August 31st.'
    } else {
        return 'Who knows. Better get on your bike.'
    }
};

$.ajax({
    url: "https://cdnjs.cloudflare.com/ajax/libs/moment.js/2.14.1/moment.min.js",
    dataType: "script",
    cache: true
}).done($("#message").html(getMessage(today)));
