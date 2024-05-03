import classNames from 'classnames';
import React, { useState } from 'react';

function SwitchBtn({painting, onClick}) {
    const [isSelected, setIsSelected] = useState(false);
    return (
        <div onClick={()=>{setIsSelected(!isSelected); onClick()}} className={classNames('flex items-center w-44 h-20 cursor-pointer bg-gray-300 rounded-full')}>
            <span className={classNames('h-20 w-20 bg-black text-4xl font-bold border text-center flex items-center justify-center rounded-full transition-all duration-200', {"ml-24 bg-white":isSelected})}>
                {isSelected ? "X" : ""}
            </span>
        </div>
    );
}

export default SwitchBtn;