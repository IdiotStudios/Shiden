let clickCount = 0;

document.addEventListener('DOMContentLoaded', () => {
    const button = document.getElementById('demoButton');
    const result = document.getElementById('clickResult');
    const statsBox = document.getElementById('statsBox');
    const clickCountSpan = document.getElementById('clickCount');
    
    const messages = [
        "Hello from JavaScript!",
        "Served by pure syscalls!",
        "No libc, no runtime!",
        "Raw machine code in action!",
        "Direct Linux syscalls!",
        "Standalone ELF binary!",
        "TCP sockets from scratch!",
        "HTTP without frameworks!"
    ];
    
    button.addEventListener('click', () => {
        clickCount++;
        const randomMessage = messages[Math.floor(Math.random() * messages.length)];
        
        button.style.transform = 'scale(0.95)';
        setTimeout(() => {
            button.style.transform = 'scale(1)';
        }, 100);

        result.textContent = randomMessage;
        result.style.opacity = '0';
        setTimeout(() => {
            result.style.opacity = '1';
        }, 50);

        statsBox.classList.remove('hidden');
        clickCountSpan.textContent = clickCount;

        if (clickCount % 5 === 0) {
            createConfetti();
        }
    });

    console.log('Shiden HTTP Server - JavaScript loaded successfully!');
    console.log('This page is served using raw Linux syscalls');
    console.log('Server implementation: socket→bind→listen→accept→write');
});

function createConfetti() {
    const colors = ['#667eea', '#764ba2', '#f093fb', '#4facfe'];
    
    for (let i = 0; i < 20; i++) {
        setTimeout(() => {
            const confetti = document.createElement('div');
            confetti.style.position = 'fixed';
            confetti.style.width = '10px';
            confetti.style.height = '10px';
            confetti.style.backgroundColor = colors[Math.floor(Math.random() * colors.length)];
            confetti.style.left = Math.random() * window.innerWidth + 'px';
            confetti.style.top = '-10px';
            confetti.style.borderRadius = '50%';
            confetti.style.pointerEvents = 'none';
            confetti.style.zIndex = '9999';
            confetti.style.transition = 'all 1s ease-out';
            
            document.body.appendChild(confetti);
            
            setTimeout(() => {
                confetti.style.top = window.innerHeight + 'px';
                confetti.style.opacity = '0';
                confetti.style.transform = `rotate(${Math.random() * 360}deg)`;
            }, 50);
            
            setTimeout(() => {
                confetti.remove();
            }, 1100);
        }, i * 50);
    }
}
