document.getElementsByClassName('side-menu-button')[0].addEventListener('click', function (e) {
    e.stopImmediatePropagation();
    e.preventDefault();

    document.querySelector('.sidebar').classList.add('active');
    return false;
});
