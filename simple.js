document.querySelector('.side-menu-button').addEventListener('click', function (e) {
    e.stopImmediatePropagation();
    e.preventDefault();

    document.querySelector('.sidebar').classList.add('active');
    return false;
});

document.querySelector('.sidebar').addEventListener('click', function (e) {
    e.stopImmediatePropagation();
    e.preventDefault();

    document.querySelector('.sidebar').classList.remove('active');
    return false;
});
